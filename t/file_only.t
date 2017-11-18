use strict;
use Filesys::Notify::Simple;
use Test::More;
use Test::SharedFork;
use File::Temp qw( tempdir );

use FindBin;

plan tests => 6;

$Filesys::Notify::Simple::interval = 0.35;
my $dir = tempdir( DIR => "$FindBin::Bin/x" );
my $test_expected = "$dir/watched.data";
my $test_unexpected = "$dir/other.data";
my $test_otherdir = "$dir/otherdir";
my $test_subdir = "$dir/subdir";
# must exist for Filesys::Notify::KQueue
open my $out, ">", $test_expected; close($out);
open my $out, ">", $test_unexpected; close($out);
my $w = Filesys::Notify::Simple->new([ $test_expected ]);

my $pid = fork;
if ($pid == 0) {
    Test::SharedFork->child;
    sleep 1;
    mkdir $test_otherdir;
    mkdir $test_subdir;
    open my $out, ">", $test_unexpected;
    print $out "foo" . time;
    close $out;
    open my $out, ">", $test_expected;
    print $out "foo" . time;
    close $out;
    sleep 1;
    open my $out, ">", $test_expected;
    print $out "bar" . time;
    close $out;
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
    like $event->{path}, qr/watched\.data/;
    like $event->{event}, qr/delete/;
    waitpid $pid, 0;
} else {
    die $!;
}


