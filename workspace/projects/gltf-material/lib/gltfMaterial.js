'use strict';
var Cesium = require('cesium');
var Promise = require('bluebird');
var async = require('async');
var path = require('path');

var defaultValue = Cesium.defaultValue;
var defined = Cesium.defined;

var loadGltfUris = require('./loadGltfUris');
var readGltf = require('./readGltf');

module.exports = {
    processJSON : processJSON,
    processFile : processFile,
    processJSONToDisk : processJSONToDisk,
    processFileToDisk : processFileToDisk
};


function processJSON(gltf, options, callback) {
    addPipelineExtras(gltf);
    loadGltfUris(gltf, options)
        .then(function() {
            processJSONWithExtras(gltf, options, function() {
                writeSources(gltf, function() {
                    callback(gltf);
                });
            });
        })
        .catch(function(err) {
            throw err;
        });
}


function processFile(inputPath, options, callback) {
    readGltf(inputPath, options, function(gltf) {
    });
}


function processJSONToDisk(gltf, outputPath, options, callback) {
    addPipelineExtras(gltf);
    loadGltfUris(gltf, options)
        .then(function() {
            processJSONWithExtras(gltf, options, function(gltf) {
                writeFile(gltf, outputPath, options, callback);
            });
        })
        .catch(function(err) {
            throw err;
        });
}

function processFileToDisk(inputPath, outputPath, options, callback) {
    readGltf(inputPath, options, function(gltf) {

    });
}

