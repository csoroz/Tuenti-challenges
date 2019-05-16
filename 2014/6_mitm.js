#!/usr/bin/env node

var net = require('net');
var util = require('util');
var crypto = require('crypto');
var fs = require('fs');

var KEYPHRASE = 'YOUR KEYPHRASE';

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function() {
  var chunk = process.stdin.read();
  if (chunk !== null) {
	kk = chunk.split('\n');
	KEYPHRASE = kk[0];
	//console.log('KEYPHRASE<'+KEYPHRASE+'>');
  }
});


var options = {
	'port': 6969,
	'host': '54.83.207.90',
};

var state = 0;

var cli, cli_secret;
var srv, srv_secret;
var keyphrase;

var socket = net.connect(options);

socket.on('data', function(data) {

	//console.log(util.format("STATE: %d", state));
	var s = data.toString();
	//console.log(s);
	var d = s.trim().split(':');
	var x = d[1].trim().split('|');
	
	//socket.write(d[1]);
	mitm(x);
	
	if (++state == 6) socket.end();
});

var mitm = function(data) {
	//console.log(data);
	if (state == 0 && data[0] == 'hello?') {
		socket.write(data[0]);
	} else if (state == 1 && data[0] == 'hello!') {
		socket.write(data[0]);
	} else if (state == 2 && data[0] == 'key') { // CLIENT->SERVER
		srv = crypto.createDiffieHellman(data[1], 'hex');
		srv.generateKeys();
		srv_secret = srv.computeSecret(data[2], 'hex');
		cli = crypto.createDiffieHellman(256);
		cli.generateKeys();
		socket.write(util.format('key|%s|%s\n', cli.getPrime('hex'), cli.getPublicKey('hex')));
	} else if (state == 3 && data[0] == 'key') { // SERVER->CLIENT
		cli_secret = cli.computeSecret(data[1], 'hex');
		var cipher = crypto.createCipheriv('aes-256-ecb', cli_secret, '');
		keyphrase = cipher.update(KEYPHRASE, 'utf8', 'hex') + cipher.final('hex');
		socket.write(util.format('key|%s\n', srv.getPublicKey('hex')));
	} else if (state == 4 && data[0] == 'keyphrase') {
		socket.write(util.format('keyphrase|%s\n', keyphrase));
	} else if (state == 5 && data[0] == 'result') {
		var cipher = crypto.createCipheriv('aes-256-ecb', srv_secret, '');
		var result = cipher.update('Thanks!', 'utf8', 'hex') + cipher.final('hex');
		socket.write(util.format('result|%s\n', result));
		var decipher = crypto.createDecipheriv('aes-256-ecb', cli_secret, '');
		var message = decipher.update(data[1], 'hex', 'utf8') + decipher.final('utf8');
		console.log(message);
	} else {
		console.log('Error');
	}
}

