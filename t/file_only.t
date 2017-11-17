use strict;
use Filesys::Notify::Simple;
use Test::More;
use Test::SharedFork;
use File::Temp qw( tempdir );

use FindBin;

plan tests => 2;

$Filesys::Notify::Simple::interval = 0.5;
my $dir = tempdir( DIR => "$FindBin::Bin/x" );
my $test_expected = "$dir/watched.data";
my $test_unexpected = "$dir/other.data";
my $test_subdir = "$dir/subdir";
# must exist for Filesys::Notify::KQueue
open my $out, ">", $test_expected; close($out);
open my $out, ">", $test_unexpected; close($out);
my $w = Filesys::Notify::Simple->new([ $test_expected ]);

my $pid = fork;
if ($pid == 0) {
    Test::SharedFork->child;
    sleep 1;
    mkdir $test_subdir;
    open my $out, ">", $test_expected;
    print $out "foo" . time;
    close $out;
    sleep 1;
    rmdir $test_subdir;
    open my $out, ">", $test_unexpected;
    print $out "foo" . time;
    close $out;
    sleep 1;
    unlink $test_expected;
    unlink $test_unexpected;
} elsif ($pid != 0) {
    Test::SharedFork->parent;
    my $event;
    alarm 10;
    $w->wait(sub { $event = shift }); # create
    like $event->{path}, qr/watched\.data/;
    $w->wait(sub { $event = shift }); # create
    like $event->{path}, qr/watched\.data/;
    waitpid $pid, 0;
} else {
    die $!;
}


