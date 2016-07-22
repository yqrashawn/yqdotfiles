'use strict';
var path = require('path');
var fs = require('fs');
var jsonfile = require('jsonfile');
var parseBinaryGltf = require('./parseBinaryGltf');
var Cesium = require('cesium');
var jquery = require('jquery');
var defined = Cesium.defined;
var DeveloperError = Cesium.DeveloperError;

module.exports = readGltf;

function readGltf(gltfPath, options, callback) {
    if (!defined(gltfPath)) {
        throw new DeveloperError('Input path is undefined.');
    }

    var fileExtension = path.extname(gltfPath);

    if (fileExtension !== '.glb' && fileExtension !== '.gltf') {
        throw new DeveloperError('Invalid glTF file.');
    }
    40
    fs.readFile(gltfPath, function(err, data) {
        if (err) {
            throw err;
        }

        var gltf;
        if (fileExtension === '.glb') {
            gltf = parseBinaryGltf(data);
        } else if (fileExtension === '.gltf') {
            gltf = JSON.parse(data);
            var newGltf = gerateMaterialArr(gltf, gltfPath, dealGltf);
        }
    });
}

function gerateMaterialArr(gltf, gltfPath, callback) {
    var nodes = gltf.nodes;
    var meshes = gltf.meshes;
    var nodeMeshMaterial = {};
    var nmmMaterials = [];
    var nmmMeshes = [];

    for (var materialId in gltf.materials) {
        nmmMaterials[materialId] = [];
    }
    for (var meshId1 in gltf.meshes) {
        nmmMeshes[meshId1] = [];
    }

    for (var nodeId in nodes) {
        nodeMeshMaterial[nodeId] = {};
        nodeMeshMaterial[nodeId].mesh = {};
        var node = nodes[nodeId];
        if (node.meshes && node.meshes.length) {
            for (var index in node.meshes) {
                var meshId = node.meshes[index];
                if (nodeMeshMaterial[nodeId].mesh[meshId] == undefined) {
                    nodeMeshMaterial[nodeId].mesh[meshId] = {};
                    nodeMeshMaterial[nodeId].mesh[meshId].material = [];
                }

            }

        }
    }

    for (var nodeId in nodes) {
        var node = nodes[nodeId];
        if (node.meshes && node.meshes.length) {
            for (var index in node.meshes) {
                var meshId = node.meshes[index];
                var primitives = meshes[meshId].primitives;
                nodeMeshMaterial[nodeId].mesh[meshId].name = meshId;
                nodeMeshMaterial[nodeId].mesh[meshId].newName = meshId + nodeId;

                for (var index in primitives) {
                    var materialName = primitives[index].material;
                }

                for (var index in primitives) {
                    var materialName = primitives[index].material;
                    nmmMeshes[meshId].push(meshId + nodeId);
                    var material = {};
                    material.name = materialName;
                    material.newName = materialName + meshId + nodeId;
                    nodeMeshMaterial[nodeId].mesh[meshId].material.push(material);
                    nmmMaterials[materialName].push(materialName + meshId + nodeId);
                }

            }

        }
    }
    callback(gltf, nodeMeshMaterial, nmmMaterials, nmmMeshes, gltfPath);
}

function dealGltf(gltf, nmm, nmmMaterials, nmmMeshes, gltfPath) {
    var nodes = gltf.nodes;
    var meshes = gltf.meshes;
    var materials = gltf.materials;

    for (var node in nmm) {

        for (var meshesIndex in nodes[node].meshes) {
            nodes[node].meshes[meshesIndex] = nmm[node].mesh[nodes[node].meshes[meshesIndex]].newName; //设置gltf-node-mesh的名字
        }

        for (var mesh in nmm[node].mesh) {

            var cloneMesh = jquery.extend(true, {}, meshes[nmm[node].mesh[mesh].name]);
            meshes[nmm[node].mesh[mesh].newName] = cloneMesh; //根据新id新建mesh
            meshes[nmm[node].mesh[mesh].newName].name = nmm[node].mesh[mesh].newName; //更改新建的mesh的name

            for (var i = 0, len = meshes[nmm[node].mesh[mesh].newName].primitives.length; i < len; i++) {
                meshes[nmm[node].mesh[mesh].newName].primitives[i].material = nmm[node].mesh[mesh].material[i].newName; //更改新建的mesh的primitive.material
            }
        }
    }

    //更新material
    for (var material in nmmMaterials) {
        for (var index in nmmMaterials[material]) {
            materials[nmmMaterials[material][index]] = materials[material];
            materials[nmmMaterials[material][index]].name = nmmMaterials[material][index];
        }
    }


    //删除多余的material mesh
    for (var material in nmmMaterials) {
        delete materials[material];
    }


    for (var meshId in nmmMeshes) {
        delete meshes[meshId];
    }

    jsonfile.writeFile(gltfPath + '.new.gltf', gltf);
    console.log('file created, path: ' + gltfPath + '.new.gltf');
}
