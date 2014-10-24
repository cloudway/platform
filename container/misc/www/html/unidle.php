<?php

list($blank, $id, $blank) = split("/", $_SERVER["PATH_INFO"]);
if (preg_match('/^[0-9a-fA-F]{24,32}$/', $id)) {
    exec("/usr/share/cloudway/libexec/cwunidle.sh $id");
    sleep(2);

    $host = $_SERVER['HTTP_HOST'];
    $proto = "http" . (isset($_SERVER['HTTPS']) ? 's' : '') . '://';
    $url = str_replace("/$id", "", $_SERVER["PATH_INFO"]);
    header("Location: $proto$host$url");
    header("Connection: close"); // Prevent the same connection from being reused - causes a redirect loop.
} else {
    error_log("Invalid id $id given to unidle.php");
    header('HTTP/1.0 403 Forbidden');
}
?>