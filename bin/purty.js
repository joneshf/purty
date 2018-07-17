#!/usr/bin/env node

const childProcess = require('child_process');
const path = require('path');

let purtyPath;

switch (process.platform) {
  case 'linux':
    purtyPath = path.join(__dirname, 'linux', 'purty');
    break;
  case 'darwin':
    purtyPath = path.join(__dirname, 'osx', 'purty');
    break;
  case 'win32':
    purtyPath = path.join(__dirname, 'win', 'purty.exe');
    break;
  default:
    purtyPath = path.join(__dirname, process.platform, 'purty');
    break;
};

const purty = childProcess.spawn(purtyPath, process.argv.slice(2));

purty.stdout.on('data', function(data) { process.stdout.write(data); });
purty.stderr.on('data', function(data) { process.stderr.write(data); });
purty.on('close', function(code) { process.exit(code); });
