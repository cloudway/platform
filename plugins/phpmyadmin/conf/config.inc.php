<?php
include 'config.sample.inc.php';

// The $cfg['Servers'] array starts with $cfg['Servers'][1]. Do not use
// $cfg['Servers'][0].
$cfg['Servers'][1]['host'] = getenv('CLOUDWAY_MYSQL_DB_HOST'); // MySQL hostname or IP address
$cfg['Servers'][1]['port'] = getenv('CLOUDWAY_MYSQL_DB_PORT'); // MySQL port - leave blank for default port

if (isset($_ENV['SESSION_TIMEOUT'])) {
	$cfg['LoginCookieValidity'] = $_ENV['SESSION_TIMEOUT'];
	ini_set('session.gc_maxlifetime', $_ENV['SESSION_TIMEOUT']);
}

$file_with_secret = 'config.inc.secret.php';

if (!file_exists($file_with_secret)) {
	$secret = hash('sha512', openssl_random_pseudo_bytes(1000));
	file_put_contents(
		$file_with_secret,
		"<?php \$cfg['blowfish_secret'] = '$secret';"
	);
}

include $file_with_secret;
