#!/usr/bin/env node

const childProcess = require('child_process');
const path = require('path');

const args = process.argv.slice(2);
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

const purty = childProcess.spawn(purtyPath, args, { stdio: 'inherit' });

purty.on('close', function(code) { process.exit(code); });
