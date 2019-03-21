# NAME

Filesys::Notify::Light - Light and simple file system watcher

# SYNOPSIS

    use Filesys::Notify::Light;

    my $watcher = Filesys::Notify::Light->new([ "." ]);
    $watcher->wait(sub {
        for my $event (@_) {
            $event->{path} # full path of the file updated
        }
    });

# DESCRIPTION

Filesys::Notify::Light is a Light but unified interface to get
notifications of changes to a given filesystem path. It utilizes
inotify2 on Linux, fsevents on OS X, kqueue on FreeBSD and
FindFirstChangeNotification on Windows if they're installed, with a
fallback to the full directory scan if they're not available.

There are some limitations in this module. If you don't like it, use
[File::ChangeNotify](http://search.cpan.org/perldoc?File::ChangeNotify).

- There is no file name based filter. Do it in your own code.
- You can not get types of events (created, updated, deleted).
- Currently `wait` method blocks.

In return, this module doesn't depend on any non-core
modules. Platform specific optimizations with [Linux::Inotify2](http://search.cpan.org/perldoc?Linux::Inotify2),
[Mac::FSEvents](http://search.cpan.org/perldoc?Mac::FSEvents), [Filesys::Notify::KQueue](http://search.cpan.org/perldoc?Filesys::Notify::KQueue) and [Win32::ChangeNotify](http://search.cpan.org/perldoc?Win32::ChangeNotify)
are truely optional.

NOTE: Using [Win32::ChangeNotify](http://search.cpan.org/perldoc?Win32::ChangeNotify) may put additional limitations.

- [Win32::ChangeNotify](http://search.cpan.org/perldoc?Win32::ChangeNotify) uses FindFirstChangeNotificationA so that
Unicode characters can not be handled.
On cygwin (1.7 or later), Unicode characters should be able to be handled
when [Win32::ChangeNotify](http://search.cpan.org/perldoc?Win32::ChangeNotify) is not used.
- If more than 64 directories are included under the specified paths,
an error occurrs.

# AUTHOR

Tatsuhiko Miyagawa <miyagawa@bulknews.net>

# LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

# SEE ALSO

[File::ChangeNotify](http://search.cpan.org/perldoc?File::ChangeNotify) [Mac::FSEvents](http://search.cpan.org/perldoc?Mac::FSEvents) [Linux::Inotify2](http://search.cpan.org/perldoc?Linux::Inotify2) [Filesys::Notify::KQueue](http://search.cpan.org/perldoc?Filesys::Notify::KQueue)
[Win32::ChangeNotify](http://search.cpan.org/perldoc?Win32::ChangeNotify)
