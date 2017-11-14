use strict;
use Filesys::Notify::Light;
use Test::More;
use Test::SharedFork;
use File::Temp qw( tempdir );

use FindBin;

plan tests => 6;

$Filesys::Notify::Light::interval = 0.35;
my $dir = tempdir( DIR => "$FindBin::Bin/x" );
my $test_expected = "$dir/watched.data";
my $test_unexpected = "$dir/other.data";
my $test_otherdir = "$dir/otherdir";
my $test_subdir = "$dir/subdir";
# must exist for Filesys::Notify::KQueue
open my $out1, ">", $test_expected; close($out1);
open my $out2, ">", $test_unexpected; close($out2);
my $w = Filesys::Notify::Light->new([ $test_expected ]);

my $pid = fork;
if ($pid == 0) {
    Test::SharedFork->child;
    sleep 1;
    mkdir $test_otherdir;
    mkdir $test_subdir;
    open my $out1, ">", $test_unexpected;
    print $out1 "foo" . time;
    close $out1;
    open my $out2, ">", $test_expected;
    print $out2 "foo" . time;
    close $out2;
    sleep 1;
    open my $out3, ">", $test_expected;
    print $out3 "bar" . time;
    close $out3;
    rmdir $test_otherdir;
    rmdir $test_subdir;
    sleep 1;
    unlink $test_unexpected;
    unlink $test_expected;
} elsif ($pid != 0) {
    Test::SharedFork->parent;
    my $event;
    alarm 10;
    $w->wait(sub { $event = shift });
    like $event->{path}, qr/watched\.data/;
    like $event->{event}, qr/modify/;
    $w->wait(sub { $event = shift });
    like $event->{path}, qr/watched\.data/;
    like $event->{event}, qr/modify/;
    $w->wait(sub { $event = shift });
    # inotify emits multiple events
    while ($event->{event} eq "modify") {
        $w->wait(sub { $event = shift });
    }
    like $event->{path}, qr/watched\.data/;
    like $event->{event}, qr/delete/;
    waitpid $pid, 0;
} else {
    die $!;
}


