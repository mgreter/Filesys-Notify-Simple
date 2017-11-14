# Copyright (c) 2013-2017 Tatsuhiko Miyagawa.
# Copyright (c) 2017-2018 Marcel Greter.

use strict;
use warnings;

use 5.008001;

use ExtUtils::MakeMaker 6.30;

################################################################################
# Emit a message to inform user of suboptimal watch behavior
# Filesys::Notify::KQueue will use inefficient polling scans
################################################################################

# stores choosen matcher
my $watchdeps = undef;

# list watchers per OS
my %watchers = (
	'linux' => [ 'Linux::Inotify2', 0.01 ],
	'cygwin' => [ 'Win32::ChangeNotify', 0.01 ],
	'mswin32' => [ 'Win32::ChangeNotify', 0.01 ],
	'darwin' => [ 'Filesys::Notify::KQueue', 0.01 ],
	'netbsd' => [ 'Filesys::Notify::KQueue', 0.01 ],
	'freebsd' => [ 'Filesys::Notify::KQueue', 0.01 ],
	'openbsd' => [ 'Filesys::Notify::KQueue', 0.01 ]
);

my $native_watcher = grep { /--native-watcher/ } @ARGV;
# check if dependency is wanted and/or available
if (exists $watchers{lc($^O)} && ! $ENV{PERL_FNS_NO_OPT}) {
	if ($native_watcher) { $watchdeps = $watchers{lc($^O)}; }
	elsif (!eval "require $watchers{lc($^O)}->[0]; return 1;") {
		warn "Consider installing $watchers{lc($^O)}->[0]\n";
		warn "Or configure with `--native-watcher` option\n";
	}
}

################################################################################
# See lib/ExtUtils/MakeMaker.pm for details of how to
# influence content of the Makefile that is written.
################################################################################

my %WriteMakefileArgs = (
	NAME               => "Filesys::Notify::Light",
	# finds $VERSION, requires EU::MM from perl >= 5.5
	VERSION_FROM       => 'lib/Filesys/Notify/Light.pm',
	# test dependencies
	TEST_REQUIRES      => {
		"File::Temp"       => 0,
		"Test::More"       => 0,
		"Test::SharedFork" => 0
	},
	# build dependencies
	BUILD_REQUIRES     => {
		'ExtUtils::MakeMaker'  => 6.30,
	},
	# build dependencies
	CONFIGURE_REQUIRES => {
		'ExtUtils::MakeMaker'  => 6.30,
	},
	# runtime dependencies
	PREREQ_PM => {
		# 'Getopt::Long'         => 0.01,
		# it doesn't have any dependencies
		# but you may want to install one of
		# Linux::Inotify2, Win32::ChangeNotify
		# Mac::FSEvents, Filesys::Notify::KQueue
		$watchdeps ? @{$watchdeps} : (),
	},
	# additional information
	META_MERGE => {
		resources => {
			license    => 'http://opensource.org/licenses/MIT',
			homepage   => 'https://metacpan.org/release/Filesys-Notify-Light',
			bugtracker => 'https://github.com/mgreter/Filesys-Notify-Light/issues',
			repository => 'https://github.com/mgreter/Filesys-Notify-Light',
		},
	},
	ABSTRACT           => "Light and simple file system watcher",
	# Original author, but now mostly rewritten completely
	AUTHOR             => q{Tatsuhiko Miyagawa <miyagawa@bulknews.net>},
	AUTHOR             => q{Marcel Greter <perl-file-notify@ocbnet.ch>},
	LICENSE            => "perl",
	EXE_FILES          => [],
);

# remove unknown key (as seen in Dist::Zilla)
unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
	my $tr = delete $WriteMakefileArgs{TEST_REQUIRES};
	my $br = $WriteMakefileArgs{BUILD_REQUIRES};
	for my $mod ( keys %$tr ) {
		if ( exists $br->{$mod} ) {
			if ($tr->{$mod} > $br->{$mod}) {
				$br->{$mod} = $tr->{$mod};
			}
		}
		else {
			$br->{$mod} = $tr->{$mod};
		}
	}
}

# remove unknown key (as seen in Dist::Zilla)
unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
	my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
	my $pp = $WriteMakefileArgs{PREREQ_PM};
	for my $mod ( keys %$br ) {
		if ( exists $pp->{$mod} ) {
			if ($br->{$mod} > $pp->{$mod}) {
				$pp->{$mod} = $br->{$mod};
			}
		}
		else {
			$pp->{$mod} = $br->{$mod};
		}
	}
}

# remove unknown key (as seen in Dist::Zilla)
unless ( eval { ExtUtils::MakeMaker->VERSION(6.52) } ) {
	delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
}

# See lib/ExtUtils/MakeMaker.pm for details of how to
# influence content of the Makefile that is written.
WriteMakefile(%WriteMakefileArgs);



