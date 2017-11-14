use strict;
use Test::More;

BEGIN { $ENV{PERL_FNS_NO_OPT} = 1; }
use Filesys::Notify::Light qw();

my $fs = Filesys::Notify::Light->new(["/xxx/nonexistent"]);
$Filesys::Notify::Light::interval = 0.5;

eval {
    $SIG{ALRM} = sub { die "Alarm\n" };
    alarm 1;
    $fs->wait(sub {});
};

is $@, "Alarm\n";

done_testing;


