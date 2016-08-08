<?php
require_once('workflows.php');

class QRCode extends Workflows {
    public function generate($query) {
        $query = urlencode( "$query" );
        $url = "http://chart.apis.google.com/chart?cht=qr&chs=300x300&chl=$query&chld=H|0";
        $line = $this->request($url);
        $image = imagecreatefromstring($line);
        imagegif($image, './qrcode.gif');
        exec('open ./qrcode.gif');
    }
}