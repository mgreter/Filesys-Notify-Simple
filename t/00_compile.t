use strict;
use Test::More tests => 2;

BEGIN { use_ok 'Filesys::Notify::Light' }

my $watcher = Filesys::Notify::Light->new([ "." ]);

ok($watcher, "File watcher instantiated");

diag "Using ", $watcher->{watcher_id}, "\n";