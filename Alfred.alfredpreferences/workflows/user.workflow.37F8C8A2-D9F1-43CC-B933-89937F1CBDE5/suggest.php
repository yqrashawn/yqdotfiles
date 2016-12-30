<?php
require_once('workflows.php');
$wf = new Workflows();

$in = $argv[1];

echo "This is $in";

$wf->result('uid', $in, 'This is '.$in.' title', 'This is '.$in.' subtitle', 'icon.png');
echo $wf->toxml();