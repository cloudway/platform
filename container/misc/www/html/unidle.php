<?php

$SVC = "com.cloudway.oddjob";
$OBJ = "/com/cloudway/oddjob";
$ITF = "com.cloudway.oddjob.unidler";

list($blank, $uuid, $blank) = split("/", $_SERVER["PATH_INFO"]);
if (preg_match('/^[0-9a-fA-F]{24,32}$/', $uuid)) {
    exec("/usr/bin/oddjob_request -s $SVC -o $OBJ -i $ITF unidle $uuid");
    sleep(2);

    $host = $_SERVER['HTTP_HOST'];
    $proto = "http" . (isset($_SERVER['HTTPS']) ? 's' : '') . '://';
    $url = str_replace("/$uuid", "", $_SERVER["PATH_INFO"]);
    header("Location: $proto$host$url");
    header("Connection: close"); // Prevent the same connection from being reused - causes a redirect loop.
} else {
    error_log("Invalid uuid $uuid given to unidle.php");
    header('HTTP/1.0 403 Forbidden');
}
?>