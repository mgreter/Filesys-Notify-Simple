package Filesys::Notify::Simple;

use strict;
use warnings;

use 5.008_001;
our $VERSION = '0.12';
our $interval = 2.0;

# list watchers per OS
our %watchers = (
    'linux' => [ 'Linux::Inotify2', \&wait_inotify2, 0.01 ],
    'cygwin' => [ 'Win32::ChangeNotify', \&wait_win32, 0.01 ],
    'mswin32' => [ 'Win32::ChangeNotify', \&wait_win32, 0.01 ],
    'darwin' => [ 'Filesys::Notify::KQueue', \&wait_kqueue, 0.01 ],
    'netbsd' => [ 'Filesys::Notify::KQueue', \&wait_kqueue, 0.01 ],
    'freebsd' => [ 'Filesys::Notify::KQueue', \&wait_kqueue, 0.01 ],
    'openbsd' => [ 'Filesys::Notify::KQueue', \&wait_kqueue, 0.01 ]
);

# flag to disable platform optimizations
use constant NO_OPT => $ENV{PERL_FNS_NO_OPT};
use constant IS_CYGWIN => $^O eq "cygwin";

# core modules
use Carp qw(croak);
use Cwd qw(abs_path realpath);
use File::Basename qw(dirname);

# create new instance
sub new
{
    my($class, $path) = @_;
    croak('Usage: Filesys::Notify::Simple->new([ $path1, $path2 ])')
        unless (ref $path eq 'ARRAY' && scalar(@_) == 2);
    my $self = bless { paths => $path }, $class;
    return $self->init;
}
# EO new

# choose file watcher
sub init
{
    my ($self) = @_;
    local $@; # preserver errors
    $self->{watcher_cb} = \&wait_timer;
    if (my $watcher = $watchers{lc $^O}) {
        if (eval { require $watcher->[0]; 1 }) {
            $self->{watcher_cb} = $watcher->[1];
        }
    }
    return $self;
}
# EO init

# main blocking wait method
sub wait
{
    my($self, $cb) = @_;
    # instantiate the platform specific watcher function once
    $self->{watcher} ||= $self->{watcher_cb}->(@{$self->{paths}});
    # call watcher function
    $self->{watcher}->($cb);
}
# EO wait

# return observers struct
sub get_observers
{
    my @path = @_;

    my (%observer, @dirs, @files, @roots);
    @path = map { abs_path($_) } @path;

    # split up watcher paths
    foreach my $path (@path) {
        if (-f $path) { push @files, $path; }
        elsif (-d $path) { push @dirs, $path; }
    }

    # get rid of inner paths
    # no need to observer twice
    foreach my $path (@dirs) {
        my $hasroot = undef;
        foreach my $root (@roots) {
            # check if path starts with root (is subpath)
            if (substr($path, 0, length($root)) eq $root) {
                $hasroot = $root;
                last;
            }
        }
        push @roots, $path unless $hasroot;
    }

    # observe directories recursively
    foreach my $path (@dirs) {
        if (exists $observer{$path}) {
            $observer{$path}->{isdir} = 1;
        } else {
            $observer{$path} = {
                files => [],
                isdir => 1
            };
        }
    }

    # observe files explicitly, but only if
    # base directory is not watched recursively
    foreach my $file (@files) {
        my $path = dirname($file);
        if (exists $observer{$path}) {
            push @{$observer{$path}->{files}}, $file;
        } else {
            $observer{$path} = {
                files => [$file],
                isdir => 0
            };
        }
    }

    # optimize scan targets for observer
    foreach my $path (keys %observer) {
        $observer{$path}->{scan} = $observer{$path}->{isdir}
            ? [ $path ] : \ @{$observer{$path}->{files}};
    }

    return %observer;
}
# EO get_observers

# NetBSD, FreeBSD, OpenBSD and Mac OSX
sub wait_kqueue {
    my @path = @_;

    my $kqueue = Filesys::Notify::KQueue->new(
        path => \@path
    );

    return sub { $kqueue->wait(shift) };
}
# EO wait_kqueue

# Windows and CYGWIN
sub mk_wait_win32 {

    return sub {
        my @path = @_;

        my %observer = get_observers(@path);

        # either scan the whole directory or only the necessary files
        my @scan = map { @{$observer{$_}->{scan}} } keys %observer;

        # get current filesystem state
        my $fs = _full_scan(@scan);

        my (@notify, @fskey);

        for my $path (keys %observer) {
            my $winpath = IS_CYGWIN ? Cygwin::posix_to_win_path($path) : $path;
            # 0x1b means 'DIR_NAME|FILE_NAME|LAST_WRITE|SIZE' = 2|1|0x10|8
            push @notify, Win32::ChangeNotify->new($winpath, $observer{$path}->{isdir}, 0x1b);
            push @fskey, $path;
        }

        return sub {
            my $cb = shift;

            my @events;
            while(1) {
                my $idx = Win32::ChangeNotify::wait_any(\@notify); 
                croak("Can't wait notifications, maybe " . scalar(@notify) . " directories exceeds limitation.") if ! defined $idx;
                if($idx > 0) {
                    --$idx;
                    # get all file changes for observed path
                    my $observer = $observer{$fskey[$idx]};
                    my $new_fs = _full_scan(@{$observer->{scan}});
                    # on windows we can only watch folders
                    # therefore we need to filter unwanted
                    # events for files we are not looking for
                    # but only if we don't watch folder itself
                    unless ($observer->{isdir}) {
                        foreach my $root (keys %{$new_fs}) {
                            # process all reported file changes in path
                            foreach my $file (keys %{$new_fs->{$root}}) {
                                # don't remove if we watch particular file
                                unless (exists $fs->{$root}->{$file}) {
                                    # we are not interested in this event
                                    delete $new_fs->{$root}->{$file};
                                }
                            }
                        }
                    }
                    $notify[$idx]->reset;
                    my $old_fs = +{ map { ($_ => $fs->{$_}) } keys %$new_fs };
                    _compare_fs($old_fs, $new_fs, sub { push @events, { path => $_[0] } });
                    $fs->{$_} = $new_fs->{$_} for keys %$new_fs;
                    last if @events; # Actually changed
                }
            }
            $cb->(@events);
        }
    }
}
# EO mk_wait_win32

# Linux
sub wait_inotify2
{
    my @path = @_;

    Linux::Inotify2->import;
    my $inotify = Linux::Inotify2->new;
    my %observer = get_observers(@path);

    my %watched;

    foreach my $observer (keys %observer) {

        my @paths;

        if ($observer{$observer}->{isdir}) {
            @paths = @{$observer{$observer}->{scan}};
            @paths = keys %{_full_scan(@paths)};
        }
        else {
            @paths = @{$observer{$observer}->{files}};
        }

        foreach my $path (@paths) {
            $inotify->watch($path, &IN_MODIFY|&IN_CREATE|&IN_DELETE|&IN_DELETE_SELF|&IN_MOVE_SELF|&IN_MOVE)
                or croak("watch failed: $!");
            $watched{$path} = 1;
        }

    }

    return sub {
        my $cb = shift;
        $inotify->blocking(1);
        my @events = $inotify->read;
        @events = map { $_->fullname } @events;
        foreach my $path (@events) {
            next if exists $watched{$path} || ! -d $path;
            $inotify->watch($path, &IN_MODIFY|&IN_CREATE|&IN_DELETE|&IN_DELETE_SELF|&IN_MOVE_SELF|&IN_MOVE)
                or croak("watch failed: $!");
            $watched{$path} = 1;
        }
        $cb->(map { +{ path => $_ } } @events);
    };
}
# EO wait_inotify2

# Pure perl fallback
sub wait_timer
{
    my @path = @_;

    my $fs = _full_scan(@path);

    return sub {
        my $cb = shift;
        my @events;
        while (1) {
            # sleep 0 is needed to fix sigalrm on windows!?
            select undef, undef, undef, $interval && sleep 0;
            my $new_fs = _full_scan(@path);
            _compare_fs($fs, $new_fs, sub { push @events, { path => $_[0] } });
            $fs = $new_fs;
            last if @events;
        };
        $cb->(@events);
    };
}
# EO wait_timer

sub _compare_fs {
    my($old, $new, $cb) = @_;

    for my $dir (keys %$old) {
        for my $path (keys %{$old->{$dir}}) {
            if (!exists $new->{$dir}{$path}) {
                $cb->($path); # deleted
            } elsif (!$new->{$dir}{$path}{is_dir} &&
                    ( $old->{$dir}{$path}{mtime} != $new->{$dir}{$path}{mtime} ||
                      $old->{$dir}{$path}{size}  != $new->{$dir}{$path}{size})) {
                $cb->($path); # updated
            }
        }
    }

    for my $dir (keys %$new) {
        for my $path (sort grep { !exists $old->{$dir}{$_} } keys %{$new->{$dir}}) {
            $cb->($path); # new
        }
    }
}

sub _full_scan {
    my @paths = @_;
    require File::Find;

    my %map;
    for my $path (@paths) {
        my $fp = eval { realpath($path) } or next;
        File::Find::finddepth({
            wanted => sub {
                my $fullname = $File::Find::fullname || File::Spec->rel2abs($File::Find::name);
                my $stat = $map{realpath($File::Find::dir)}{$fullname} = _stat($fullname);
                $map{$path}{$fullname} = $stat if $stat->{is_dir}; # keep track of directories
            },
            follow_fast => 1,
            follow_skip => 2,
            no_chdir => 1,
        }, $path);

        # remove root entry
        # NOTE: On MSWin32, realpath and rel2abs disagree with path separator.
        delete $map{$fp}{File::Spec->rel2abs($fp)} if exists $map{$fp};
    }

    return \%map;
}

sub _stat {
    my $path = shift;
    my @stat = stat $path;
    return { path => $path, mtime => $stat[9], size => $stat[7], is_dir => -d _ };
}


1;
__END__

=encoding utf-8

=for stopwords

=head1 NAME

Filesys::Notify::Simple - Simple file system watcher

=head1 SYNOPSIS

  use Filesys::Notify::Simple;

  my $watcher = Filesys::Notify::Simple->new([ "." ]);
  $watcher->wait(sub {
      for my $event (@_) {
          $event->{path} # full path of the file updated
      }
  });

=head1 DESCRIPTION

Filesys::Notify::Simple is a simple but unified interface to get
notifications of changes to a given filesystem path or for specific
files. It utilizes inotify2 on Linux, Win32::ChangeNotify on Windows,
KQueue on FreeBSD, OpenBSD, NetBSD and Mac OSX if they're installed,
with a fallback to a full directory scan if they're not available.

There are some limitations in this module. If you don't like it, use
L<File::ChangeNotify>.

=over 4

=item *

You can not get types of events (created, updated, deleted).

=item *

Currently C<wait> method blocks.

=back

In return, this module doesn't depend on any non-core modules.
Platform specific optimizations with L<Linux::Inotify2>,
L<Win32::ChangeNotify> or L<Filesys::Notify::KQueue> are optional.

NOTE: Using L<Win32::ChangeNotify> may put additional limitations.

=over 4

=item *

L<Win32::ChangeNotify> uses FindFirstChangeNotificationA (the A stands
for ANSI) so Unicode characters are not supported. On cygwin (1.7 or
later), Unicode characters should be able to be handled when
L<Win32::ChangeNotify> is not used.

=item *

If more than 64 directories are included under the specified paths,
an error occurs due to a limitation with the Windows API.

=back

=head1 AUTHOR

Tatsuhiko Miyagawa E<lt>miyagawa@bulknews.netE<gt>

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

L<File::ChangeNotify> L<Mac::FSEvents> L<Linux::Inotify2> L<Filesys::Notify::KQueue>
L<Win32::ChangeNotify>

=cut

# directories are watched recursively