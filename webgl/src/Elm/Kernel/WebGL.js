/*

import Elm.Kernel.VirtualDom exposing (custom, doc)
import WebGL.Internal as WI exposing (enableSetting, enableOption)

*/

function _WebGL_log(/* msg */) {
  // console.log(msg);
}

var _WebGL_guid = 0;

function _WebGL_listEach(fn, list) {
  for (; list.b; list = list.b) {
    fn(list.a);
  }
}

function _WebGL_listLength(list) {
  var length = 0;
  for (; list.b; list = list.b) {
    length++;
  }
  return length;
}

var _WebGL_rAF = typeof requestAnimationFrame !== 'undefined' ?
  requestAnimationFrame :
  function (cb) { setTimeout(cb, 1000 / 60); };

// eslint-disable-next-line no-unused-vars
var _WebGL_entity = F5(function (settings, vert, frag, mesh, uniforms) {
  return {
    $: __0_ENTITY,
    __settings: settings,
    __vert: vert,
    __frag: frag,
    __mesh: mesh,
    __uniforms: uniforms
  };
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableBlend = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.blend;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.BLEND);
    glSetting.enabled = true;
  }

  // a   b   c   d   e   f   g h i j
  // eq1 f11 f12 eq2 f21 f22 r g b a
  if (!current || (current.a !== setting.a || current.d !== setting.d)) {
    gl.blendEquationSeparate(setting.a, setting.d);
  }
  if (!current || (current.b !== setting.b || current.c !== setting.c || current.e !== setting.e || current.f !== setting.f)) {
    gl.blendFuncSeparate(setting.b, setting.c, setting.e, setting.f);
  }
  if (!current || (current.g !== setting.g || current.h !== setting.h || current.i !== setting.i || current.j !== setting.j)) {
    gl.blendColor(setting.g, setting.h, setting.i, setting.j);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableDepthTest = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.depthTest;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.DEPTH_TEST);
    glSetting.enabled = true;
  }

  // a    b    c    d
  // func mask near far
  if (!current || (current.a !== setting.a)) {
    gl.depthFunc(setting.a);
  }
  if (!current || (current.b !== setting.b)) {
    gl.depthMask(setting.b);
  }
  if (!current || (current.c !== setting.c || current.d !== setting.d)) {
    gl.depthRange(setting.c, setting.d);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableStencilTest = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.stencilTest;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.STENCIL_TEST);
    glSetting.enabled = true;
  }

  // a   b    c         d     e     f      g      h     i     j      k
  // ref mask writeMask test1 fail1 zfail1 zpass1 test2 fail2 zfail2 zpass2
  if (!current || (current.d !== setting.d || current.a !== setting.a || current.b !== setting.b)) {
    gl.stencilFuncSeparate(gl.FRONT, setting.d, setting.a, setting.b);
  }
  if (!current || (current.e !== setting.e || current.f !== setting.f || current.g !== setting.g)) {
    gl.stencilOpSeparate(gl.FRONT, setting.e, setting.f, setting.g);
  }
  if (!current || (current.c !== setting.c)) {
    gl.stencilMask(setting.c);
  }
  if (!current || (current.h !== setting.h || current.a !== setting.a || current.b !== setting.b)) {
    gl.stencilFuncSeparate(gl.BACK, setting.h, setting.a, setting.b);
  }
  if (!current || (current.i !== setting.i || current.j !== setting.j || current.k !== setting.k)) {
    gl.stencilOpSeparate(gl.BACK, setting.i, setting.j, setting.k);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableScissor = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.scissor;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.SCISSOR_TEST);
    glSetting.enabled = true;
  }

  if (!current || (current.a !== setting.a || current.b !== setting.b || current.c !== setting.c || current.d !== setting.d)) {
    gl.scissor(setting.a, setting.b, setting.c, setting.d);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableColorMask = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.colorMask;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;
  glSetting.enabled = true;

  if (!current || (current.a !== setting.a || current.b !== setting.b || current.c !== setting.c || current.d !== setting.d)) {
    gl.colorMask(setting.a, setting.b, setting.c, setting.d);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableCullFace = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.cullFace;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.CULL_FACE);
    glSetting.enabled = true;
  }

  if (!current || (current.a !== setting.a)) {
    gl.cullFace(setting.a);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enablePolygonOffset = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.polygonOffset;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.POLYGON_OFFSET_FILL);
    glSetting.enabled = true;
  }

  if (!current || (current.a !== setting.a || current.b !== setting.b)) {
    gl.polygonOffset(setting.a, setting.b);
  }
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableSampleCoverage = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.sampleCoverage;
  var current = glSetting.setting;
  glSetting.toggle = glSettings.toggle;
  glSetting.setting = setting;

  if (!glSetting.enabled) {
    gl.enable(gl.SAMPLE_COVERAGE);
    glSetting.enabled = true;
  }

  if (!current || (current.a !== setting.a || current.b !== setting.b)) {
    gl.sampleCoverage(setting.a, setting.b);
  }
});

var _WebGL_enableSampleAlphaToCoverage = F3(function (gl, glSettings, setting) {
  var glSetting = glSettings.sampleAlphaToCoverage;
  glSetting.toggle = glSettings.toggle;

  if (!glSetting.enabled) {
    gl.enable(gl.SAMPLE_ALPHA_TO_COVERAGE);
    glSetting.enabled = true;
  }
});

var _WebGL_disableBlend = function (cache) {
  cache.gl.disable(cache.gl.BLEND);
};

var _WebGL_disableDepthTest = function (cache) {
  cache.gl.disable(cache.gl.DEPTH_TEST);
  cache.gl.depthMask(true);
  var setting = cache.glSettings.stencilTest.setting;
  cache.glSettings.depthTest.setting = {
    a: setting.a,
    b: true,
    c: setting.c,
    d: setting.d
  }
};

var _WebGL_disableStencilTest = function (cache) {
  cache.gl.disable(cache.gl.STENCIL_TEST);
  cache.gl.stencilMask(cache.STENCIL_WRITEMASK);
  var setting = cache.glSettings.stencilTest.setting;
  cache.glSettings.stencilTest.setting = {
    a: setting.a,
    b: setting.b,
    c: cache.STENCIL_WRITEMASK,
    d: setting.d,
    e: setting.e,
    f: setting.f,
    g: setting.g,
    h: setting.h,
    i: setting.i,
    j: setting.j,
    k: setting.k
  }
};

var _WebGL_disableScissor = function (cache) {
  cache.gl.disable(cache.gl.SCISSOR_TEST);
};

var _WebGL_disableColorMask = function (cache) {
  cache.gl.colorMask(true, true, true, true);
  cache.glSettings.colorMask.setting = {
    a: true,
    b: true,
    c: true,
    d: true
  };
};

var _WebGL_disableCullFace = function (cache) {
  cache.gl.disable(cache.gl.CULL_FACE);
};

var _WebGL_disablePolygonOffset = function (cache) {
  cache.gl.disable(cache.gl.POLYGON_OFFSET_FILL);
};

var _WebGL_disableSampleCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_COVERAGE);
};

var _WebGL_disableSampleAlphaToCoverage = function (cache) {
  cache.gl.disable(cache.gl.SAMPLE_ALPHA_TO_COVERAGE);
};

var _WebGL_settingsNames = ['blend', 'depthTest', 'stencilTest', 'scissor', 'colorMask', 'cullFace', 'polygonOffset', 'sampleCoverage', 'sampleAlphaToCoverage']
var _WebGL_settingsDisableFunctions = [_WebGL_disableBlend, _WebGL_disableDepthTest, _WebGL_disableStencilTest, _WebGL_disableScissor, _WebGL_disableColorMask, _WebGL_disableCullFace, _WebGL_disablePolygonOffset, _WebGL_disableSampleCoverage, _WebGL_disableSampleAlphaToCoverage]

function _WebGL_doCompile(gl, src, type) {

  var shader = gl.createShader(type);
  _WebGL_log('Created shader');

  gl.shaderSource(shader, src);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    throw gl.getShaderInfoLog(shader);
  }

  return shader;

}

function _WebGL_doLink(gl, vshader, fshader) {

  var program = gl.createProgram();
  _WebGL_log('Created program');

  gl.attachShader(program, vshader);
  gl.attachShader(program, fshader);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    throw gl.getProgramInfoLog(program);
  }

  return program;

}

function _WebGL_getAttributeInfo(gl, type) {
  switch (type) {
    case gl.FLOAT:
      return { size: 1, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC2:
      return { size: 2, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC3:
      return { size: 3, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_VEC4:
      return { size: 4, arraySize: 1, type: Float32Array, baseType: gl.FLOAT };
    case gl.FLOAT_MAT4:
      return { size: 4, arraySize: 4, type: Float32Array, baseType: gl.FLOAT };
    case gl.INT:
      return { size: 1, arraySize: 1, type: Int32Array, baseType: gl.INT };
  }
}

/**
 *  Form the buffer for a given attribute.
 *
 *  @param {WebGLRenderingContext} gl context
 *  @param {WebGLActiveInfo} attribute the attribute to bind to.
 *         We use its name to grab the record by name and also to know
 *         how many elements we need to grab.
 *  @param {Mesh} mesh The mesh coming in from Elm.
 *  @param {Object} attributes The mapping between the attribute names and Elm fields
 *  @return {WebGLBuffer}
 */
function _WebGL_doBindAttribute(gl, attribute, mesh, attributes) {
  // The length of the number of vertices that
  // complete one 'thing' based on the drawing mode.
  // ie, 2 for Lines, 3 for Triangles, etc.
  var elemSize = mesh.a.__$elemSize;

  var idxKeys = [];
  for (var i = 0; i < elemSize; i++) {
    idxKeys.push(String.fromCharCode(97 + i));
  }

  function dataFill(data, cnt, fillOffset, elem, key) {
    var i;
    if (elemSize === 1) {
      for (i = 0; i < cnt; i++) {
        data[fillOffset++] = cnt === 1 ? elem[key] : elem[key][i];
      }
    } else {
      idxKeys.forEach(function (idx) {
        for (i = 0; i < cnt; i++) {
          data[fillOffset++] = cnt === 1 ? elem[idx][key] : elem[idx][key][i];
        }
      });
    }
  }

  var attributeInfo = _WebGL_getAttributeInfo(gl, attribute.type);

  if (attributeInfo === undefined) {
    throw new Error('No info available for: ' + attribute.type);
  }

  var dataIdx = 0;
  var dataOffset = attributeInfo.size * attributeInfo.arraySize * elemSize;
  var array = new attributeInfo.type(_WebGL_listLength(mesh.b) * dataOffset);

  _WebGL_listEach(function (elem) {
    dataFill(array, attributeInfo.size * attributeInfo.arraySize, dataIdx, elem, attributes[attribute.name] || attribute.name);
    dataIdx += dataOffset;
  }, mesh.b);

  var buffer = gl.createBuffer();
  _WebGL_log('Created attribute buffer ' + attribute.name);

  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);
  return buffer;
}

/**
 *  This sets up the binding caching buffers.
 *
 *  We don't actually bind any buffers now except for the indices buffer.
 *  The problem with filling the buffers here is that it is possible to
 *  have a buffer shared between two webgl shaders;
 *  which could have different active attributes. If we bind it here against
 *  a particular program, we might not bind them all. That final bind is now
 *  done right before drawing.
 *
 *  @param {WebGLRenderingContext} gl context
 *  @param {Mesh} mesh a mesh object from Elm
 *  @return {Object} buffer - an object with the following properties
 *  @return {Number} buffer.numIndices
 *  @return {WebGLBuffer|null} buffer.indexBuffer - optional index buffer
 *  @return {Object} buffer.buffers - will be used to buffer attributes
 */
function _WebGL_doBindSetup(gl, mesh) {
  if (mesh.a.__$indexSize > 0) {
    _WebGL_log('Created index buffer');
    var indexBuffer = gl.createBuffer();
    var indices = _WebGL_makeIndexedBuffer(mesh.c, mesh.a.__$indexSize);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
    return {
      numIndices: indices.length,
      indexBuffer: indexBuffer,
      buffers: {}
    };
  } else {
    return {
      numIndices: mesh.a.__$elemSize * _WebGL_listLength(mesh.b),
      indexBuffer: null,
      buffers: {}
    };
  }
}

/**
 *  Create an indices array and fill it from indices
 *  based on the size of the index
 *
 *  @param {List} indicesList the list of indices
 *  @param {Number} indexSize the size of the index
 *  @return {Uint16Array} indices
 */
function _WebGL_makeIndexedBuffer(indicesList, indexSize) {
  var indices = new Uint16Array(_WebGL_listLength(indicesList) * indexSize);
  var fillOffset = 0;
  var i;
  _WebGL_listEach(function (elem) {
    if (indexSize === 1) {
      indices[fillOffset++] = elem;
    } else {
      for (i = 0; i < indexSize; i++) {
        indices[fillOffset++] = elem[String.fromCharCode(97 + i)];
      }
    }
  }, indicesList);
  return indices;
}

function _WebGL_getProgID(vertID, fragID) {
  return vertID + '#' + fragID;
}

var _WebGL_drawGL = F2(function (model, domNode) {

  var gl = model.__cache.gl;
  var glSettings = model.__cache.glSettings;

  if (!gl) {
    return domNode;
  }

  gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT | gl.STENCIL_BUFFER_BIT);
  _WebGL_log('Drawing');

  function drawEntity(entity) {
    if (!entity.__mesh.b.b) {
      return; // Empty list
    }

    var progid;
    var program;
    if (entity.__vert.id && entity.__frag.id) {
      progid = _WebGL_getProgID(entity.__vert.id, entity.__frag.id);
      program = model.__cache.programs[progid];
    }

    if (!program) {

      var vshader;
      if (entity.__vert.id) {
        vshader = model.__cache.shaders[entity.__vert.id];
      } else {
        entity.__vert.id = _WebGL_guid++;
      }

      if (!vshader) {
        vshader = _WebGL_doCompile(gl, entity.__vert.src, gl.VERTEX_SHADER);
        model.__cache.shaders[entity.__vert.id] = vshader;
      }

      var fshader;
      if (entity.__frag.id) {
        fshader = model.__cache.shaders[entity.__frag.id];
      } else {
        entity.__frag.id = _WebGL_guid++;
      }

      if (!fshader) {
        fshader = _WebGL_doCompile(gl, entity.__frag.src, gl.FRAGMENT_SHADER);
        model.__cache.shaders[entity.__frag.id] = fshader;
      }

      var glProgram = _WebGL_doLink(gl, vshader, fshader);

      program = {
        glProgram: glProgram,
        attributes: Object.assign({}, entity.__vert.attributes, entity.__frag.attributes),
        currentUniforms: {},
        activeAttributes: [],
        activeAttributeLocations: []
      };

      var numActiveAttributes = gl.getProgramParameter(glProgram, gl.ACTIVE_ATTRIBUTES);
      for (var i = 0; i < numActiveAttributes; i++) {
        var attribute = gl.getActiveAttrib(glProgram, i);
        var attribLocation = gl.getAttribLocation(glProgram, attribute.name);
        program.activeAttributes.push(attribute);
        program.activeAttributeLocations.push(attribLocation);
      }

      program.uniformSetters = _WebGL_createUniformSetters(
        gl,
        model,
        program,
        Object.assign({}, entity.__vert.uniforms, entity.__frag.uniforms)
      );

      progid = _WebGL_getProgID(entity.__vert.id, entity.__frag.id);
      model.__cache.programs[progid] = program;

    }

    gl.useProgram(program.glProgram);

    _WebGL_setUniforms(program.uniformSetters, entity.__uniforms);

    var buffer = model.__cache.buffers.get(entity.__mesh);

    if (!buffer) {
      buffer = _WebGL_doBindSetup(gl, entity.__mesh);
      model.__cache.buffers.set(entity.__mesh, buffer);
    }

    for (var i = 0; i < program.activeAttributes.length; i++) {
      var attribute = program.activeAttributes[i];
      var attribLocation = program.activeAttributeLocations[i];

      if (buffer.buffers[attribute.name] === undefined) {
        buffer.buffers[attribute.name] = _WebGL_doBindAttribute(gl, attribute, entity.__mesh, program.attributes);
      }
      gl.bindBuffer(gl.ARRAY_BUFFER, buffer.buffers[attribute.name]);

      var attributeInfo = _WebGL_getAttributeInfo(gl, attribute.type);
      if (attributeInfo.arraySize === 1) {
        gl.enableVertexAttribArray(attribLocation);
        gl.vertexAttribPointer(attribLocation, attributeInfo.size, attributeInfo.baseType, false, 0, 0);
      } else {
        // Point to four vec4 in case of mat4
        var offset = attributeInfo.size * 4; // float32 takes 4 bytes
        var stride = offset * attributeInfo.arraySize;
        for (var m = 0; m < attributeInfo.arraySize; m++) {
          gl.enableVertexAttribArray(attribLocation + m);
          gl.vertexAttribPointer(attribLocation + m, attributeInfo.size, attributeInfo.baseType, false, stride, offset * m);
        }
      }
    }

    model.__cache.glSettings.toggle = !model.__cache.glSettings.toggle;

    _WebGL_listEach(A2(__WI_enableSetting, gl, glSettings), entity.__settings);

    for (var st = 0; st < _WebGL_settingsNames.length; st++) {
      var glSetting = glSettings[_WebGL_settingsNames[st]];
      if (glSetting.toggle !== glSettings.toggle && glSetting.enabled) {
        _WebGL_settingsDisableFunctions[st](model.__cache);
        glSetting.enabled = false;
        glSetting.toggle = glSettings.toggle;
      }
    }

    if (buffer.indexBuffer) {
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer.indexBuffer);
      gl.drawElements(entity.__mesh.a.__$mode, buffer.numIndices, gl.UNSIGNED_SHORT, 0);
    } else {
      gl.drawArrays(entity.__mesh.a.__$mode, 0, buffer.numIndices);
    }
  }

  _WebGL_listEach(drawEntity, model.__entities);
  return domNode;
});

function _WebGL_createUniformSetters(gl, model, program, uniformsMap) {
  var glProgram = program.glProgram;
  var currentUniforms = program.currentUniforms;
  var textureCounter = 0;
  function createUniformSetter(glProgram, uniform) {
    var uniformName = uniform.name;
    var uniformLocation = gl.getUniformLocation(glProgram, uniformName);
    switch (uniform.type) {
      case gl.INT:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1i(uniformLocation, value);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1f(uniformLocation, value);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC2:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform2f(uniformLocation, value[0], value[1]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC3:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform3f(uniformLocation, value[0], value[1], value[2]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_VEC4:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform4f(uniformLocation, value[0], value[1], value[2], value[3]);
            currentUniforms[uniformName] = value;
          }
        };
      case gl.FLOAT_MAT4:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniformMatrix4fv(uniformLocation, false, new Float32Array(value));
            currentUniforms[uniformName] = value;
          }
        };
      case gl.SAMPLER_2D:
        var currentTexture = textureCounter++;
        return function (texture) {
          if (currentUniforms[uniformName] !== value) {
            gl.activeTexture(gl.TEXTURE0 + currentTexture);
            var tex = model.__cache.textures.get(texture);
            if (!tex) {
              _WebGL_log('Created texture');
              tex = texture.__$createTexture(gl);
              model.__cache.textures.set(texture, tex);
            }
            gl.bindTexture(gl.TEXTURE_2D, tex);
            gl.uniform1i(uniformLocation, currentTexture);
            currentUniforms[uniformName] = texture;
          }
        };
      case gl.BOOL:
        return function (value) {
          if (currentUniforms[uniformName] !== value) {
            gl.uniform1i(uniformLocation, value);
            currentUniforms[uniformName] = texture;
          }
        };
      default:
        _WebGL_log('Unsupported uniform type: ' + uniform.type);
        return function () { };
    }
  }

  var uniformSetters = {};
  var numUniforms = gl.getProgramParameter(glProgram, gl.ACTIVE_UNIFORMS);
  for (var i = 0; i < numUniforms; i++) {
    var uniform = gl.getActiveUniform(glProgram, i);
    uniformSetters[uniformsMap[uniform.name] || uniform.name] = createUniformSetter(glProgram, uniform);
  }

  return uniformSetters;
}

function _WebGL_setUniforms(setters, values) {
  Object.keys(values).forEach(function (name) {
    var setter = setters[name];
    if (setter) {
      setter(values[name]);
    }
  });
}

// VIRTUAL-DOM WIDGET

// eslint-disable-next-line no-unused-vars
var _WebGL_toHtml = F3(function (options, factList, entities) {
  return __VirtualDom_custom(
    factList,
    {
      __entities: entities,
      __cache: {},
      __options: options
    },
    _WebGL_render,
    _WebGL_diff
  );
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableAlpha = F2(function (options, option) {
  options.contextAttributes.alpha = true;
  options.contextAttributes.premultipliedAlpha = option.a;
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableDepth = F2(function (options, option) {
  options.contextAttributes.depth = true;
  options.sceneSettings.push(function (gl) {
    gl.clearDepth(option.a);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableStencil = F2(function (options, option) {
  options.contextAttributes.stencil = true;
  options.sceneSettings.push(function (gl) {
    gl.clearStencil(option.a);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableAntialias = F2(function (options, option) {
  options.contextAttributes.antialias = true;
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enableClearColor = F2(function (options, option) {
  options.sceneSettings.push(function (gl) {
    gl.clearColor(option.a, option.b, option.c, option.d);
  });
});

// eslint-disable-next-line no-unused-vars
var _WebGL_enablePreserveDrawingBuffer = F2(function (options, option) {
  options.contextAttributes.preserveDrawingBuffer = true;
});

/**
 *  Creates canvas and schedules initial _WebGL_drawGL
 *  @param {Object} model
 *  @param {Object} model.__cache that may contain the following properties:
           gl, shaders, programs, buffers, textures
 *  @param {List<Option>} model.__options list of options coming from Elm
 *  @param {List<Entity>} model.__entities list of entities coming from Elm
 *  @return {HTMLElement} <canvas> if WebGL is supported, otherwise a <div>
 */
function _WebGL_render(model) {
  var options = {
    contextAttributes: {
      alpha: false,
      depth: false,
      stencil: false,
      antialias: false,
      premultipliedAlpha: false,
      preserveDrawingBuffer: false
    },
    sceneSettings: []
  };

  _WebGL_listEach(__WI_enableOption(options), model.__options);

  _WebGL_log('Render canvas');
  var canvas = __VirtualDom_doc.createElement('canvas');
  var gl = canvas.getContext && (
    canvas.getContext('webgl', options.contextAttributes) ||
    canvas.getContext('experimental-webgl', options.contextAttributes)
  );

  if (gl && typeof WeakMap !== 'undefined') {
    options.sceneSettings.forEach(function (sceneSetting) {
      sceneSetting(gl);
    });

    model.__cache.gl = gl;
    model.__cache.glSettings = {
      toggle: false, // used to distinguish settings from previous and current draw calls
      blend: { setting: undefined, enabled: false, toggle: false },
      depthTest: { setting: undefined, enabled: false, toggle: false },
      stencilTest: { setting: undefined, enabled: false, toggle: false },
      scissor: { setting: undefined, enabled: false, toggle: false },
      colorMask: { setting: undefined, enabled: false, toggle: false },
      cullFace: { setting: undefined, enabled: false, toggle: false },
      polygonOffset: { setting: undefined, enabled: false, toggle: false },
      sampleCoverage: { setting: undefined, enabled: false, toggle: false },
      sampleAlphaToCoverage: { enabled: false, toggle: false }
    };

    model.__cache.shaders = [];
    model.__cache.programs = {};
    model.__cache.buffers = new WeakMap();
    model.__cache.textures = new WeakMap();
    // Memorize the initial stencil write mask, because
    // browsers may have different number of stencil bits
    model.__cache.STENCIL_WRITEMASK = gl.getParameter(gl.STENCIL_WRITEMASK);

    // Render for the first time.
    // This has to be done in animation frame,
    // because the canvas is not in the DOM yet
    _WebGL_rAF(function () {
      return A2(_WebGL_drawGL, model, canvas);
    });

  } else {
    canvas = __VirtualDom_doc.createElement('div');
    canvas.innerHTML = '<a href="https://get.webgl.org/">Enable WebGL</a> to see this content!';
  }

  return canvas;
}

function _WebGL_diff(oldModel, newModel) {
  newModel.__cache = oldModel.__cache;
  return _WebGL_drawGL(newModel);
}
