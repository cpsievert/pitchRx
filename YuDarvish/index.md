





<script src="CanvasMatrix.js" type="text/javascript"></script>
<canvas id="webgl1textureCanvas" style="display: none;" width="256" height="256">
<img src="webgl1snapshot.png" alt="webgl1snapshot" width=1441/><br>
	Your browser does not support the HTML5 canvas element.</canvas>
<!-- ****** spheres object 6 ****** -->
<script id="webgl1vshader6" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec3 aNorm;
	uniform mat4 normMatrix;
	varying vec3 vNormal;
	void main(void) {
	  vPosition = mvMatrix * vec4(aPos, 1.);
	  gl_Position = prMatrix * vPosition;
	  vCol = aCol;
	  vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);
	}
</script>
<script id="webgl1fshader6" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec3 vNormal;
	vec3 eye = normalize(-vPosition.xyz);
	const vec3 emission = vec3(0., 0., 0.);
	const vec3 ambient1 = vec3(0., 0., 0.);
	const vec3 specular1 = vec3(1., 1., 1.);// light*material
	const float shininess1 = 50.;
	vec4 colDiff1 = vec4(vCol.rgb * vec3(1., 1., 1.), vCol.a);
	const vec3 lightDir1 = vec3(0., 0., 1.);
	vec3 halfVec1 = normalize(lightDir1 + eye);
	void main(void) {
      vec4 lighteffect = vec4(emission, 0.);
	  vec3 n = normalize(vNormal);
	  n = -faceforward(n, n, eye);
	  vec3 col1 = ambient1;
	  float nDotL1 = dot(n, lightDir1);
	  col1 = col1 + max(nDotL1, 0.) * colDiff1.rgb;
	  col1 = col1 + pow(max(dot(halfVec1, n), 0.), shininess1) * specular1;
	  lighteffect = lighteffect + vec4(col1, colDiff1.a);
	  gl_FragColor = lighteffect;
	}
</script> 
<!-- ****** lines object 7 ****** -->
<script id="webgl1vshader7" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	void main(void) {
	  vPosition = mvMatrix * vec4(aPos, 1.);
	  gl_Position = prMatrix * vPosition;
	  vCol = aCol;
	}
</script>
<script id="webgl1fshader7" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  gl_FragColor = lighteffect;
	}
</script> 
<!-- ****** text object 8 ****** -->
<script id="webgl1vshader8" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader8" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<!-- ****** lines object 9 ****** -->
<script id="webgl1vshader9" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	void main(void) {
	  vPosition = mvMatrix * vec4(aPos, 1.);
	  gl_Position = prMatrix * vPosition;
	  vCol = aCol;
	}
</script>
<script id="webgl1fshader9" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  gl_FragColor = lighteffect;
	}
</script> 
<!-- ****** text object 10 ****** -->
<script id="webgl1vshader10" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader10" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<!-- ****** lines object 11 ****** -->
<script id="webgl1vshader11" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	void main(void) {
	  vPosition = mvMatrix * vec4(aPos, 1.);
	  gl_Position = prMatrix * vPosition;
	  vCol = aCol;
	}
</script>
<script id="webgl1fshader11" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  gl_FragColor = lighteffect;
	}
</script> 
<!-- ****** text object 12 ****** -->
<script id="webgl1vshader12" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader12" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<!-- ****** text object 13 ****** -->
<script id="webgl1vshader13" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader13" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<!-- ****** text object 14 ****** -->
<script id="webgl1vshader14" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader14" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<!-- ****** text object 15 ****** -->
<script id="webgl1vshader15" type="x-shader/x-vertex">
	attribute vec3 aPos;
	attribute vec4 aCol;
	uniform mat4 mvMatrix;
	uniform mat4 prMatrix;
	varying vec4 vCol;
	varying vec4 vPosition;
	attribute vec2 aTexcoord;
	varying vec2 vTexcoord;
	attribute vec2 aOfs;
	void main(void) {
	  vCol = aCol;
	  vTexcoord = aTexcoord;
	  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);
	  pos = pos/pos.w;
	  gl_Position = pos + vec4(aOfs, 0.,0.);
	}
</script>
<script id="webgl1fshader15" type="x-shader/x-fragment"> 
	#ifdef GL_ES
	precision highp float;
	#endif
	varying vec4 vCol; // carries alpha
	varying vec4 vPosition;
	varying vec2 vTexcoord;
	uniform sampler2D uSampler;
    vec4 colDiff = vCol;
	void main(void) {
	  vec4 lighteffect = colDiff;
	  vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);
	  if (textureColor.a < 0.1)
	    discard;
	  else
	    gl_FragColor = textureColor;
	}
</script> 
<script type="text/javascript"> 
	function getShader ( gl, id ){
	   var shaderScript = document.getElementById ( id );
	   var str = "";
	   var k = shaderScript.firstChild;
	   while ( k ){
	     if ( k.nodeType == 3 ) str += k.textContent;
	     k = k.nextSibling;
	   }
	   var shader;
	   if ( shaderScript.type == "x-shader/x-fragment" )
             shader = gl.createShader ( gl.FRAGMENT_SHADER );
	   else if ( shaderScript.type == "x-shader/x-vertex" )
             shader = gl.createShader(gl.VERTEX_SHADER);
	   else return null;
	   gl.shaderSource(shader, str);
	   gl.compileShader(shader);
	   if (gl.getShaderParameter(shader, gl.COMPILE_STATUS) == 0)
	     alert(gl.getShaderInfoLog(shader));
	   return shader;
	}
	var min = Math.min;
	var max = Math.max;
	var sqrt = Math.sqrt;
	var sin = Math.sin;
	var acos = Math.acos;
	var tan = Math.tan;
	var SQRT2 = Math.SQRT2;
	var PI = Math.PI;
	var log = Math.log;
	var exp = Math.exp;
	function webgl1webGLStart() {
	   var debug = function(msg) {
	     document.getElementById("webgl1debug").innerHTML = msg;
	   }
	   debug("");
	   var canvas = document.getElementById("webgl1canvas");
	   if (!window.WebGLRenderingContext){
	     debug("<img src=\"webgl1snapshot.png\" alt=\"webgl1snapshot\" width=1441/><br> Your browser does not support WebGL. See <a href=\"http://get.webgl.org\">http://get.webgl.org</a>");
	     return;
	   }
	   var gl;
	   try {
	     // Try to grab the standard context. If it fails, fallback to experimental.
	     gl = canvas.getContext("webgl") 
	       || canvas.getContext("experimental-webgl");
	   }
	   catch(e) {}
	   if ( !gl ) {
	     debug("<img src=\"webgl1snapshot.png\" alt=\"webgl1snapshot\" width=1441/><br> Your browser appears to support WebGL, but did not create a WebGL context.  See <a href=\"http://get.webgl.org\">http://get.webgl.org</a>");
	     return;
	   }
	   var width = 1441;  var height = 505;
	   canvas.width = width;   canvas.height = height;
	   gl.viewport(0, 0, width, height);
	   var prMatrix = new CanvasMatrix4();
	   var mvMatrix = new CanvasMatrix4();
	   var normMatrix = new CanvasMatrix4();
	   var saveMat = new CanvasMatrix4();
	   saveMat.makeIdentity();
	   var distance;
	   var zoom = 1;
	   var fov = 30;
	   var userMatrix = new CanvasMatrix4();
	   userMatrix.load([
	    1, 0, 0, 0,
	    0, 0.3420201, -0.9396926, 0,
	    0, 0.9396926, 0.3420201, 0,
	    0, 0, 0, 1
		]);
	   function getPowerOfTwo(value) {
	     var pow = 1;
	     while(pow<value) {
	       pow *= 2;
	     }
	     return pow;
	   }
	   function handleLoadedTexture(texture, textureCanvas) {
	     gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
	     gl.bindTexture(gl.TEXTURE_2D, texture);
	     gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, textureCanvas);
	     gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
	     gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);
	     gl.generateMipmap(gl.TEXTURE_2D);
	     gl.bindTexture(gl.TEXTURE_2D, null);
	   }
	   function loadImageToTexture(filename, texture) {   
	     var canvas = document.getElementById("webgl1textureCanvas");
	     var ctx = canvas.getContext("2d");
	     var image = new Image();
	     image.onload = function() {
	       var w = image.width;
	       var h = image.height;
	       var canvasX = getPowerOfTwo(w);
	       var canvasY = getPowerOfTwo(h);
	       canvas.width = canvasX;
	       canvas.height = canvasY;
	       ctx.imageSmoothingEnabled = true;
	       ctx.drawImage(image, 0, 0, canvasX, canvasY);
	       handleLoadedTexture(texture, canvas);
   	       drawScene();
	     }
	     image.src = filename;
	   }  	   
	   function drawTextToCanvas(text, cex) {
	     var canvasX, canvasY;
	     var textX, textY;
	     var textHeight = 20 * cex;
	     var textColour = "white";
	     var fontFamily = "Arial";
	     var backgroundColour = "rgba(0,0,0,0)";
	     var canvas = document.getElementById("webgl1textureCanvas");
	     var ctx = canvas.getContext("2d");
	     ctx.font = textHeight+"px "+fontFamily;
             canvasX = 1;
             var widths = [];
	     for (var i = 0; i < text.length; i++)  {
	       widths[i] = ctx.measureText(text[i]).width;
	       canvasX = (widths[i] > canvasX) ? widths[i] : canvasX;
	     }	  
	     canvasX = getPowerOfTwo(canvasX);
	     var offset = 2*textHeight; // offset to first baseline
	     var skip = 2*textHeight;   // skip between baselines	  
	     canvasY = getPowerOfTwo(offset + text.length*skip);
	     canvas.width = canvasX;
	     canvas.height = canvasY;
	     ctx.fillStyle = backgroundColour;
	     ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
	     ctx.fillStyle = textColour;
	     ctx.textAlign = "left";
	     ctx.textBaseline = "alphabetic";
	     ctx.font = textHeight+"px "+fontFamily;
	     for(var i = 0; i < text.length; i++) {
	       textY = i*skip + offset;
	       ctx.fillText(text[i], 0,  textY);
	     }
	     return {canvasX:canvasX, canvasY:canvasY,
	             widths:widths, textHeight:textHeight,
	             offset:offset, skip:skip};
	   }
	   // ****** sphere object ******
	   var v=new Float32Array([
	    -1, 0, 0,
	    1, 0, 0,
	    0, -1, 0,
	    0, 1, 0,
	    0, 0, -1,
	    0, 0, 1,
	    -0.7071068, 0, -0.7071068,
	    -0.7071068, -0.7071068, 0,
	    0, -0.7071068, -0.7071068,
	    -0.7071068, 0, 0.7071068,
	    0, -0.7071068, 0.7071068,
	    -0.7071068, 0.7071068, 0,
	    0, 0.7071068, -0.7071068,
	    0, 0.7071068, 0.7071068,
	    0.7071068, -0.7071068, 0,
	    0.7071068, 0, -0.7071068,
	    0.7071068, 0, 0.7071068,
	    0.7071068, 0.7071068, 0,
	    -0.9349975, 0, -0.3546542,
	    -0.9349975, -0.3546542, 0,
	    -0.77044, -0.4507894, -0.4507894,
	    0, -0.3546542, -0.9349975,
	    -0.3546542, 0, -0.9349975,
	    -0.4507894, -0.4507894, -0.77044,
	    -0.3546542, -0.9349975, 0,
	    0, -0.9349975, -0.3546542,
	    -0.4507894, -0.77044, -0.4507894,
	    -0.9349975, 0, 0.3546542,
	    -0.77044, -0.4507894, 0.4507894,
	    0, -0.9349975, 0.3546542,
	    -0.4507894, -0.77044, 0.4507894,
	    -0.3546542, 0, 0.9349975,
	    0, -0.3546542, 0.9349975,
	    -0.4507894, -0.4507894, 0.77044,
	    -0.9349975, 0.3546542, 0,
	    -0.77044, 0.4507894, -0.4507894,
	    0, 0.9349975, -0.3546542,
	    -0.3546542, 0.9349975, 0,
	    -0.4507894, 0.77044, -0.4507894,
	    0, 0.3546542, -0.9349975,
	    -0.4507894, 0.4507894, -0.77044,
	    -0.77044, 0.4507894, 0.4507894,
	    0, 0.3546542, 0.9349975,
	    -0.4507894, 0.4507894, 0.77044,
	    0, 0.9349975, 0.3546542,
	    -0.4507894, 0.77044, 0.4507894,
	    0.9349975, -0.3546542, 0,
	    0.9349975, 0, -0.3546542,
	    0.77044, -0.4507894, -0.4507894,
	    0.3546542, -0.9349975, 0,
	    0.4507894, -0.77044, -0.4507894,
	    0.3546542, 0, -0.9349975,
	    0.4507894, -0.4507894, -0.77044,
	    0.9349975, 0, 0.3546542,
	    0.77044, -0.4507894, 0.4507894,
	    0.3546542, 0, 0.9349975,
	    0.4507894, -0.4507894, 0.77044,
	    0.4507894, -0.77044, 0.4507894,
	    0.9349975, 0.3546542, 0,
	    0.77044, 0.4507894, -0.4507894,
	    0.4507894, 0.4507894, -0.77044,
	    0.3546542, 0.9349975, 0,
	    0.4507894, 0.77044, -0.4507894,
	    0.77044, 0.4507894, 0.4507894,
	    0.4507894, 0.77044, 0.4507894,
	    0.4507894, 0.4507894, 0.77044
	   ]);
	   var f=new Uint16Array([
	    0, 18, 19,
	    6, 20, 18,
	    7, 19, 20,
	    19, 18, 20,
	    4, 21, 22,
	    8, 23, 21,
	    6, 22, 23,
	    22, 21, 23,
	    2, 24, 25,
	    7, 26, 24,
	    8, 25, 26,
	    25, 24, 26,
	    7, 20, 26,
	    6, 23, 20,
	    8, 26, 23,
	    26, 20, 23,
	    0, 19, 27,
	    7, 28, 19,
	    9, 27, 28,
	    27, 19, 28,
	    2, 29, 24,
	    10, 30, 29,
	    7, 24, 30,
	    24, 29, 30,
	    5, 31, 32,
	    9, 33, 31,
	    10, 32, 33,
	    32, 31, 33,
	    9, 28, 33,
	    7, 30, 28,
	    10, 33, 30,
	    33, 28, 30,
	    0, 34, 18,
	    11, 35, 34,
	    6, 18, 35,
	    18, 34, 35,
	    3, 36, 37,
	    12, 38, 36,
	    11, 37, 38,
	    37, 36, 38,
	    4, 22, 39,
	    6, 40, 22,
	    12, 39, 40,
	    39, 22, 40,
	    6, 35, 40,
	    11, 38, 35,
	    12, 40, 38,
	    40, 35, 38,
	    0, 27, 34,
	    9, 41, 27,
	    11, 34, 41,
	    34, 27, 41,
	    5, 42, 31,
	    13, 43, 42,
	    9, 31, 43,
	    31, 42, 43,
	    3, 37, 44,
	    11, 45, 37,
	    13, 44, 45,
	    44, 37, 45,
	    11, 41, 45,
	    9, 43, 41,
	    13, 45, 43,
	    45, 41, 43,
	    1, 46, 47,
	    14, 48, 46,
	    15, 47, 48,
	    47, 46, 48,
	    2, 25, 49,
	    8, 50, 25,
	    14, 49, 50,
	    49, 25, 50,
	    4, 51, 21,
	    15, 52, 51,
	    8, 21, 52,
	    21, 51, 52,
	    15, 48, 52,
	    14, 50, 48,
	    8, 52, 50,
	    52, 48, 50,
	    1, 53, 46,
	    16, 54, 53,
	    14, 46, 54,
	    46, 53, 54,
	    5, 32, 55,
	    10, 56, 32,
	    16, 55, 56,
	    55, 32, 56,
	    2, 49, 29,
	    14, 57, 49,
	    10, 29, 57,
	    29, 49, 57,
	    14, 54, 57,
	    16, 56, 54,
	    10, 57, 56,
	    57, 54, 56,
	    1, 47, 58,
	    15, 59, 47,
	    17, 58, 59,
	    58, 47, 59,
	    4, 39, 51,
	    12, 60, 39,
	    15, 51, 60,
	    51, 39, 60,
	    3, 61, 36,
	    17, 62, 61,
	    12, 36, 62,
	    36, 61, 62,
	    17, 59, 62,
	    15, 60, 59,
	    12, 62, 60,
	    62, 59, 60,
	    1, 58, 53,
	    17, 63, 58,
	    16, 53, 63,
	    53, 58, 63,
	    3, 44, 61,
	    13, 64, 44,
	    17, 61, 64,
	    61, 44, 64,
	    5, 55, 42,
	    16, 65, 55,
	    13, 42, 65,
	    42, 55, 65,
	    16, 63, 65,
	    17, 64, 63,
	    13, 65, 64,
	    65, 63, 64
	   ]);
	   var sphereBuf = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, sphereBuf);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var sphereIbuf = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, sphereIbuf);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   // ****** spheres object 6 ******
	   var prog6  = gl.createProgram();
	   gl.attachShader(prog6, getShader( gl, "webgl1vshader6" ));
	   gl.attachShader(prog6, getShader( gl, "webgl1fshader6" ));
	   gl.linkProgram(prog6);
	   var v=new Float32Array([
	    -2.421, 50, 5.121, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -2.358, 50, 5.159, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.428, 50, 5.201, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.393, 50, 5.265, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.319, 50, 5.382, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.279, 50, 5.352, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -3.062, 50, 5.104, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.478, 50, 4.821, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.694, 50, 4.988, 0, 0.6509804, 0, 1, 0.12,
	    -2.344456, 48.65675, 5.092121, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -2.267117, 48.60083, 5.100117, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.354282, 48.60796, 5.146576, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.318416, 48.62485, 5.21646, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.23524, 48.60426, 5.328973, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.178364, 48.58921, 5.31998, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -3.028788, 48.77289, 5.104425, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.366932, 48.65848, 4.702471, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.630805, 48.6998, 4.970786, 0, 0.6509804, 0, 1, 0.12,
	    -2.269305, 47.31652, 5.061123, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -2.177188, 47.2051, 5.040268, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.281049, 47.21939, 5.091383, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.244644, 47.25335, 5.16702, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.151959, 47.2123, 5.275313, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.078916, 47.18244, 5.28694, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.994093, 47.54837, 5.101801, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.255789, 47.31963, 4.582423, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.567562, 47.40231, 4.950903, 0, 0.6509804, 0, 1, 0.12,
	    -2.195545, 45.97931, 5.028007, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -2.088213, 45.8128, 4.979453, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.208301, 45.8343, 5.035422, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.171684, 45.88551, 5.116681, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.069158, 45.82413, 5.22102, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.980656, 45.77967, 5.252881, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.957915, 46.32644, 5.096128, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.144569, 45.98344, 4.460856, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.504269, 46.10753, 4.928352, 0, 0.6509804, 0, 1, 0.12,
	    -2.123178, 44.64512, 4.992772, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -2.000192, 44.42393, 4.917672, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.136037, 44.45269, 4.978692, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.099535, 44.52132, 5.065442, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.986837, 44.43975, 5.166093, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.883584, 44.38091, 5.217801, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.920253, 45.10709, 5.087406, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.033274, 44.64992, 4.337771, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.440926, 44.81545, 4.903133, 0, 0.6509804, 0, 1, 0.12,
	    -2.052202, 43.31395, 4.955419, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.913125, 43.03849, 4.854925, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.064257, 43.07455, 4.921194, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.028199, 43.16079, 5.013302, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.904995, 43.05917, 5.110532, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.7877, 42.98615, 5.181701, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.881108, 43.89033, 5.075634, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.921904, 43.31907, 4.213168, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.377535, 43.52608, 4.875245, 0, 0.6509804, 0, 1, 0.12,
	    -1.98262, 41.98581, 4.915947, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.827012, 41.65648, 4.791212, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.992963, 41.69989, 4.862927, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.957674, 41.80391, 4.960264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.823633, 41.68237, 5.054339, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.693004, 41.59541, 5.144582, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.840479, 42.67616, 5.060812, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.810457, 41.99088, 4.087045, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.314094, 42.23941, 4.844689, 0, 0.6509804, 0, 1, 0.12,
	    -1.914429, 40.66068, 4.874357, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.741853, 40.27789, 4.726533, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.922153, 40.3287, 4.803892, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.887962, 40.45069, 4.906325, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.74275, 40.30936, 4.997512, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.599496, 40.20867, 5.106442, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.798367, 41.46457, 5.042942, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.698935, 40.66535, 3.959404, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.250605, 40.95545, 4.811464, 0, 0.6509804, 0, 1, 0.12,
	    -1.84763, 39.33857, 4.830648, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.657648, 38.90274, 4.660888, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.851827, 38.96099, 4.744088, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.819061, 39.10112, 4.851486, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.662347, 38.94014, 4.940051, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.507176, 38.82595, 5.067283, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.754771, 40.25556, 5.022022, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.587338, 39.34249, 3.830245, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.187066, 39.67419, 4.775571, 0, 0.6509804, 0, 1, 0.12,
	    -1.782224, 38.01949, 4.784821, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.574397, 37.53103, 4.594277, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.781986, 37.59676, 4.683516, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.750972, 37.75521, 4.795748, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.582424, 37.5747, 4.881958, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.416044, 37.44723, 5.027104, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.709692, 39.04914, 4.998054, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.475664, 38.02229, 3.699567, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.123477, 38.39565, 4.73701, 0, 0.6509804, 0, 1, 0.12,
	    -1.71821, 36.70342, 4.736875, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.4921, 36.16274, 4.5267, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.71263, 36.236, 4.622175, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.683695, 36.41296, 4.73911, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.50298, 36.21306, 4.82323, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.3261, 36.07251, 4.985905, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.66313, 37.84531, 4.971035, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.363915, 36.70477, 3.56737, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.05984, 37.1198, 4.69578, 0, 0.6509804, 0, 1, 0.12,
	    -1.655588, 35.39037, 4.686811, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.410757, 34.79788, 4.458157, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.643758, 34.87872, 4.560066, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.61723, 35.07435, 4.681572, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.424016, 34.85521, 4.763869, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.237344, 34.70181, 4.943686, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.615084, 36.64407, 4.940968, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.25209, 35.3899, 3.433655, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.996153, 35.84667, 4.651882, 0, 0.6509804, 0, 1, 0.12,
	    -1.594358, 34.08035, 4.634628, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.330368, 33.43646, 4.388648, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.575371, 33.52492, 4.497188, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.551577, 33.73941, 4.623135, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.345531, 33.50114, 4.703875, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.149776, 33.33512, 4.900447, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.565555, 35.44542, 4.90785, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.14019, 34.0777, 3.298421, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.932418, 34.57624, 4.605315, 0, 0.6509804, 0, 1, 0.12,
	    -1.534521, 32.77334, 4.580327, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.250933, 32.07846, 4.318173, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.507469, 32.17459, 4.433542, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.486736, 32.40812, 4.563797, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.267526, 32.15087, 4.643248, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.063396, 31.97243, 4.856188, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.514543, 34.24934, 4.871684, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.028213, 32.76817, 3.161668, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.868633, 33.30851, 4.55608, 0, 0.6509804, 0, 1, 0.12,
	    -1.476076, 31.46936, 4.523907, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.172452, 30.7239, 4.246732, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.440051, 30.82774, 4.369127, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.422706, 31.08048, 4.50356, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.190001, 30.80438, 4.581987, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.978204, 30.61375, 4.81091, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.462047, 33.05586, 4.832469, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.9161614, 31.4613, 3.023397, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.804798, 32.0435, 4.504177, 0, 0.6509804, 0, 1, 0.12,
	    -1.419022, 30.16839, 4.465369, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.094925, 29.37276, 4.174325, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.373117, 29.48436, 4.303944, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.359489, 29.7565, 4.442422, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.112955, 29.46169, 4.520092, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8942, 29.25908, 4.764611, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.408067, 31.86496, 4.790204, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.8040338, 30.1571, 2.883607, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.740915, 30.78119, 4.449605, 0, 0.6509804, 0, 1, 0.12,
	    -1.363362, 28.87045, 4.404712, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -1.018352, 28.02506, 4.100952, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.306669, 28.14446, 4.237992, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.297083, 28.43617, 4.380385, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.036389, 28.12278, 4.457565, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.811384, 27.90842, 4.717293, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.352605, 30.67665, 4.74489, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.6918304, 28.85556, 2.742299, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.676982, 29.52158, 4.392365, 0, 0.6509804, 0, 1, 0.12,
	    -1.309093, 27.57553, 4.341937, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.942733, 26.68079, 4.026613, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.240705, 26.80804, 4.171272, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.23549, 27.1195, 4.317449, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9603022, 26.78766, 4.394404, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.729756, 26.56177, 4.668954, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.295659, 29.49092, 4.696526, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.5795513, 27.55669, 2.599472, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.613001, 28.26468, 4.332456, 0, 0.6509804, 0, 1, 0.12,
	    -1.256216, 26.28362, 4.277043, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.868068, 25.33995, 3.951308, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.175225, 25.47509, 4.103783, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.174708, 25.80649, 4.253613, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8846952, 25.45633, 4.330609, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.649316, 25.21912, 4.619596, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.237229, 28.30778, 4.645113, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.4671966, 26.26048, 2.455127, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.54897, 27.01049, 4.269879, 0, 0.6509804, 0, 1, 0.12,
	    -1.204732, 24.99474, 4.210031, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.794357, 24.00254, 3.875037, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.11023, 24.14562, 4.035526, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.114738, 24.49713, 4.188876, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8095678, 24.12879, 4.266181, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.570064, 23.88049, 4.569218, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.177316, 27.12723, 4.590652, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.3547662, 24.96694, 2.309263, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.484889, 25.759, 4.204634, 0, 0.6509804, 0, 1, 0.12,
	    -1.15464, 23.70888, 4.1409, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.7216, 22.66856, 3.7978, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.04572, 22.81962, 3.9665, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.05558, 23.19142, 4.12324, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.73492, 22.80504, 4.20112, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.492, 22.54586, 4.51782, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.11592, 25.94926, 4.53314, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.24226, 23.67606, 2.16188, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.42076, 24.51022, 4.13672, 0, 0.6509804, 0, 1, 0.12,
	    -1.10594, 22.42604, 4.069651, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.649797, 21.33801, 3.719597, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9816943, 21.4971, 3.896706, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9972339, 21.88937, 4.056704, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6607518, 21.48508, 4.135425, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.415124, 21.21524, 4.465402, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.05304, 24.77388, 4.472579, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.1296781, 22.38785, 2.012979, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.356581, 23.26414, 4.066138, 0, 0.6509804, 0, 1, 0.12,
	    -1.058632, 21.14622, 3.996283, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.578948, 20.01089, 3.640428, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9181532, 20.17806, 3.826143, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9396998, 20.59097, 3.989268, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5870632, 20.16891, 4.069097, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.339436, 19.88863, 4.411964, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.988677, 23.60108, 4.408969, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.0170206, 21.1023, 1.862559, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.292354, 22.02078, 3.992887, 0, 0.6509804, 0, 1, 0.12,
	    -1.012717, 19.86942, 3.920797, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.509053, 18.68721, 3.560293, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8550967, 18.86249, 3.754812, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8829775, 19.29623, 3.920933, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5138542, 18.85652, 4.002136, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.264936, 18.56602, 4.357506, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.922831, 22.43088, 4.34231, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.09571265, 19.81942, 1.71062, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.228077, 20.78011, 3.916968, 0, 0.6509804, 0, 1, 0.12,
	    -0.9681936, 18.59563, 3.843192, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.440112, 17.36695, 3.479192, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7925248, 17.5504, 3.682712, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8270672, 18.00515, 3.851698, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4411248, 17.54793, 3.934541, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.191624, 17.24743, 4.302029, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.855501, 21.26325, 4.272602, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.2085216, 18.53921, 1.557163, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.16375, 19.54216, 3.838381, 0, 0.6509804, 0, 1, 0.12,
	    -0.9250625, 17.32487, 3.763469, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.372125, 16.05013, 3.397125, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7304375, 16.24178, 3.609844, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7719687, 16.71772, 3.781563, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.368875, 16.24313, 3.866313, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1195, 15.93284, 4.245531, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.786687, 20.09822, 4.199844, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.3214062, 17.26166, 1.402187, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.099375, 18.30691, 3.757125, 0, 0.6509804, 0, 1, 0.12,
	    -0.8833236, 16.05713, 3.681627, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.305092, 14.73673, 3.314092, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6688348, 14.93664, 3.536207, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7176822, 15.43394, 3.710528, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2971048, 14.94211, 3.797451, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.048564, 14.62227, 4.188014, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.716391, 18.93577, 4.124037, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.4343666, 15.98677, 1.245693, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.03495, 17.07436, 3.673201, 0, 0.6509804, 0, 1, 0.12,
	    -0.8429769, 14.79242, 3.597667, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.239013, 13.42677, 3.230093, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6077167, 13.63498, 3.461802, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6642076, 14.15382, 3.638593, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2258142, 13.64488, 3.727956, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.021184, 13.3157, 4.129477, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.644611, 17.77591, 4.04518, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.5474026, 14.71455, 1.08768, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9704766, 15.84452, 3.586608, 0, 0.6509804, 0, 1, 0.12,
	    -0.8040224, 13.53072, 3.511588, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.173888, 12.12023, 3.145128, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5470832, 12.33679, 3.386628, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6115448, 12.87736, 3.565758, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1550032, 12.35145, 3.657827, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.089744, 12.01313, 4.069919, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.571347, 16.61863, 3.963274, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.6605144, 13.445, 0.9281488, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9059536, 14.61739, 3.497347, 0, 0.6509804, 0, 1, 0.12,
	    -0.7664601, 12.27204, 3.423391, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.109717, 10.81713, 3.059197, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4869343, 11.04208, 3.310686, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5596939, 11.60455, 3.492024, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.0846718, 11.0618, 3.587065, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.157116, 10.71458, 4.009342, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.4966, 15.46394, 3.878319, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.7737018, 12.17811, 0.7670987, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8413814, 13.39297, 3.405418, 0, 0.6509804, 0, 1, 0.12,
	    -0.73029, 11.01638, 3.333075, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    -0.0465, 9.51746, 2.9723, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.42727, 9.750845, 3.233975, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.508655, 10.33539, 3.41739, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.01482, 9.77594, 3.51567, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.2233, 9.420035, 3.947745, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.42037, 14.31184, 3.790315, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.886965, 10.91389, 0.60453, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.77676, 12.17124, 3.31082, 0, 0.6509804, 0, 1, 0.12,
	    -0.6955121, 9.763742, 3.240641, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.015763, 8.221219, 2.884437, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3680903, 8.463086, 3.156496, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4584279, 9.069896, 3.341856, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.0545522, 8.493871, 3.443641, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.288296, 8.129498, 3.885128, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.342656, 13.16232, 3.699261, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.000304, 9.652327, 0.4404427, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7120894, 10.95223, 3.213554, 0, 0.6509804, 0, 1, 0.12,
	    -0.6621264, 8.514125, 3.146088, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.077072, 6.92841, 2.795608, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3093952, 7.178803, 3.078248, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4090128, 7.808051, 3.265422, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.1234448, 7.21559, 3.370979, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.352104, 6.842969, 3.821491, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.263459, 12.01539, 3.605158, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.113718, 8.393434, 0.2748368, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6473696, 9.735923, 3.113619, 0, 0.6509804, 0, 1, 0.12,
	    -0.6301329, 7.267528, 3.049417, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.137427, 5.63903, 2.705813, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2511847, 5.897996, 2.999232, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3604096, 6.549862, 3.188089, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.1918578, 5.941099, 3.297684, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.414724, 5.560449, 3.756835, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.182779, 10.87104, 3.508006, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.227209, 7.137206, 0.1077123, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5826006, 8.522322, 3.011016, 0, 0.6509804, 0, 1, 0.12,
	    -0.5995316, 6.023951, 2.950627, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.196828, 4.353082, 2.615052, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1934588, 4.620666, 2.919447, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3126182, 5.295328, 3.109856, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.2597912, 4.670398, 3.223755, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.476156, 4.281938, 3.691158, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.100615, 9.729281, 3.407804, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.340775, 5.883644, -0.0609308, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5177824, 7.311426, 2.905745, 0, 0.6509804, 0, 1, 0.12,
	    -0.5703225, 4.783395, 2.849719, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.255275, 3.070565, 2.523325, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1362175, 3.346811, 2.838894, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2656387, 4.044449, 3.030723, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.327245, 3.403485, 3.149193, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.5364, 3.007434, 3.624461, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.016968, 8.590109, 3.304554, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.454416, 4.632746, -0.2310925, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.452915, 6.103236, 2.797805, 0, 0.6509804, 0, 1, 0.12,
	    -0.5425056, 3.545859, 2.746692, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.312768, 1.791478, 2.430632, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.0794608, 2.076433, 2.757572, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2194712, 2.797225, 2.95069, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.3942192, 2.140362, 3.073997, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.595456, 1.736938, 3.556745, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9318368, 7.453522, 3.198254, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.568134, 3.384514, -0.4027728, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3879984, 4.897753, 2.687197, 0, 0.6509804, 0, 1, 0.12,
	    -0.5160809, 2.311344, 2.641547, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1741156, 1.553656, 2.869757, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8452227, 6.319522, 3.088904, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.681927, 2.138948, -0.5759717, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3230326, 3.694975, 2.57392, 0, 0.6509804, 0, 1, 0.12,
	    -0.4977693, 1.417, 2.563886, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1691729, 1.417, 2.860794, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7571252, 5.188108, 2.976506, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.748029, 1.417, -0.6772271, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2580176, 2.494904, 2.457975, 0, 0.6509804, 0, 1, 0.12,
	    -0.4977693, 1.417, 2.563886, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1691729, 1.417, 2.860794, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6675443, 4.059281, 2.861057, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.748029, 1.417, -0.6772271, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1994537, 1.417, 2.351328, 0, 0.6509804, 0, 1, 0.12,
	    -0.4977693, 1.417, 2.563886, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1691729, 1.417, 2.860794, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.57648, 2.93304, 2.74256, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.748029, 1.417, -0.6772271, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1994537, 1.417, 2.351328, 0, 0.6509804, 0, 1, 0.12,
	    -0.4977693, 1.417, 2.563886, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1691729, 1.417, 2.860794, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4839323, 1.809385, 2.621013, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.748029, 1.417, -0.6772271, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1994537, 1.417, 2.351328, 0, 0.6509804, 0, 1, 0.12,
	    -0.4977693, 1.417, 2.563886, 0.9254902, 0.6941177, 0.4627451, 1, 0.12,
	    0.3294486, 1.417, 2.403264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.05012951, 1.417, 2.714967, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1691729, 1.417, 2.860794, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4324479, 1.417, 3.030546, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6101694, 1.417, 3.539497, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4512138, 1.417, 2.577783, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.748029, 1.417, -0.6772271, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1994537, 1.417, 2.351328, 0, 0.6509804, 0, 1, 0.12
	   ]);
	   var values6 = v;
	   var posLoc6 = gl.getAttribLocation(prog6, "aPos");
	   var colLoc6 = gl.getAttribLocation(prog6, "aCol");
	   var normLoc6 = gl.getAttribLocation(prog6, "aNorm");
	   var mvMatLoc6 = gl.getUniformLocation(prog6,"mvMatrix");
	   var prMatLoc6 = gl.getUniformLocation(prog6,"prMatrix");
	   var normMatLoc6 = gl.getUniformLocation(prog6,"normMatrix");
	   // ****** lines object 7 ******
	   var prog7  = gl.createProgram();
	   gl.attachShader(prog7, getShader( gl, "webgl1vshader7" ));
	   gl.attachShader(prog7, getShader( gl, "webgl1fshader7" ));
	   gl.linkProgram(prog7);
	   var v=new Float32Array([
	    -3, 0.5646551, -0.8917155,
	    1, 0.5646551, -0.8917155,
	    -3, 0.5646551, -0.8917155,
	    -3, -0.6925371, -1.053921,
	    -2, 0.5646551, -0.8917155,
	    -2, -0.6925371, -1.053921,
	    -1, 0.5646551, -0.8917155,
	    -1, -0.6925371, -1.053921,
	    0, 0.5646551, -0.8917155,
	    0, -0.6925371, -1.053921,
	    1, 0.5646551, -0.8917155,
	    1, -0.6925371, -1.053921
	   ]);
	   var posLoc7 = gl.getAttribLocation(prog7, "aPos");
	   var colLoc7 = gl.getAttribLocation(prog7, "aCol");
	   var buf7 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf7);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var mvMatLoc7 = gl.getUniformLocation(prog7,"mvMatrix");
	   var prMatLoc7 = gl.getUniformLocation(prog7,"prMatrix");
	   // ****** text object 8 ******
	   var prog8  = gl.createProgram();
	   gl.attachShader(prog8, getShader( gl, "webgl1vshader8" ));
	   gl.attachShader(prog8, getShader( gl, "webgl1fshader8" ));
	   gl.linkProgram(prog8);
	   var texts = [
	    "-3",
	    "-2",
	    "-1",
	    " 0",
	    " 1"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX8 = texinfo.canvasX;
	   var canvasY8 = texinfo.canvasY;
	   var ofsLoc8 = gl.getAttribLocation(prog8, "aOfs");
	   var texture8 = gl.createTexture();
	   var texLoc8 = gl.getAttribLocation(prog8, "aTexcoord");
	   var sampler8 = gl.getUniformLocation(prog8,"uSampler");
    	   handleLoadedTexture(texture8, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -3, -3.206922, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3, -3.206922, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3, -3.206922, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3, -3.206922, -1.378331, 0, 1.5, 0.5, 0.5,
	    -2, -3.206922, -1.378331, 0, -0.5, 0.5, 0.5,
	    -2, -3.206922, -1.378331, 1, -0.5, 0.5, 0.5,
	    -2, -3.206922, -1.378331, 1, 1.5, 0.5, 0.5,
	    -2, -3.206922, -1.378331, 0, 1.5, 0.5, 0.5,
	    -1, -3.206922, -1.378331, 0, -0.5, 0.5, 0.5,
	    -1, -3.206922, -1.378331, 1, -0.5, 0.5, 0.5,
	    -1, -3.206922, -1.378331, 1, 1.5, 0.5, 0.5,
	    -1, -3.206922, -1.378331, 0, 1.5, 0.5, 0.5,
	    0, -3.206922, -1.378331, 0, -0.5, 0.5, 0.5,
	    0, -3.206922, -1.378331, 1, -0.5, 0.5, 0.5,
	    0, -3.206922, -1.378331, 1, 1.5, 0.5, 0.5,
	    0, -3.206922, -1.378331, 0, 1.5, 0.5, 0.5,
	    1, -3.206922, -1.378331, 0, -0.5, 0.5, 0.5,
	    1, -3.206922, -1.378331, 1, -0.5, 0.5, 0.5,
	    1, -3.206922, -1.378331, 1, 1.5, 0.5, 0.5,
	    1, -3.206922, -1.378331, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<5; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc8 = gl.getAttribLocation(prog8, "aPos");
	   var colLoc8 = gl.getAttribLocation(prog8, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3,
	    4, 5, 6, 4, 6, 7,
	    8, 9, 10, 8, 10, 11,
	    12, 13, 14, 12, 14, 15,
	    16, 17, 18, 16, 18, 19
	   ]);
	   var buf8 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf8);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf8 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf8);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc8 = gl.getUniformLocation(prog8,"mvMatrix");
	   var prMatLoc8 = gl.getUniformLocation(prog8,"prMatrix");
	   // ****** lines object 9 ******
	   var prog9  = gl.createProgram();
	   gl.attachShader(prog9, getShader( gl, "webgl1vshader9" ));
	   gl.attachShader(prog9, getShader( gl, "webgl1fshader9" ));
	   gl.linkProgram(prog9);
	   var v=new Float32Array([
	    -3.25775, 10, -0.8917155,
	    -3.25775, 50, -0.8917155,
	    -3.25775, 10, -0.8917155,
	    -3.387789, 10, -1.053921,
	    -3.25775, 20, -0.8917155,
	    -3.387789, 20, -1.053921,
	    -3.25775, 30, -0.8917155,
	    -3.387789, 30, -1.053921,
	    -3.25775, 40, -0.8917155,
	    -3.387789, 40, -1.053921,
	    -3.25775, 50, -0.8917155,
	    -3.387789, 50, -1.053921
	   ]);
	   var posLoc9 = gl.getAttribLocation(prog9, "aPos");
	   var colLoc9 = gl.getAttribLocation(prog9, "aCol");
	   var buf9 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf9);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var mvMatLoc9 = gl.getUniformLocation(prog9,"mvMatrix");
	   var prMatLoc9 = gl.getUniformLocation(prog9,"prMatrix");
	   // ****** text object 10 ******
	   var prog10  = gl.createProgram();
	   gl.attachShader(prog10, getShader( gl, "webgl1vshader10" ));
	   gl.attachShader(prog10, getShader( gl, "webgl1fshader10" ));
	   gl.linkProgram(prog10);
	   var texts = [
	    "10",
	    "20",
	    "30",
	    "40",
	    "50"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX10 = texinfo.canvasX;
	   var canvasY10 = texinfo.canvasY;
	   var ofsLoc10 = gl.getAttribLocation(prog10, "aOfs");
	   var texture10 = gl.createTexture();
	   var texLoc10 = gl.getAttribLocation(prog10, "aTexcoord");
	   var sampler10 = gl.getUniformLocation(prog10,"uSampler");
    	   handleLoadedTexture(texture10, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -3.647865, 10, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3.647865, 10, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3.647865, 10, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3.647865, 10, -1.378331, 0, 1.5, 0.5, 0.5,
	    -3.647865, 20, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3.647865, 20, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3.647865, 20, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3.647865, 20, -1.378331, 0, 1.5, 0.5, 0.5,
	    -3.647865, 30, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3.647865, 30, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3.647865, 30, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3.647865, 30, -1.378331, 0, 1.5, 0.5, 0.5,
	    -3.647865, 40, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3.647865, 40, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3.647865, 40, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3.647865, 40, -1.378331, 0, 1.5, 0.5, 0.5,
	    -3.647865, 50, -1.378331, 0, -0.5, 0.5, 0.5,
	    -3.647865, 50, -1.378331, 1, -0.5, 0.5, 0.5,
	    -3.647865, 50, -1.378331, 1, 1.5, 0.5, 0.5,
	    -3.647865, 50, -1.378331, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<5; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc10 = gl.getAttribLocation(prog10, "aPos");
	   var colLoc10 = gl.getAttribLocation(prog10, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3,
	    4, 5, 6, 4, 6, 7,
	    8, 9, 10, 8, 10, 11,
	    12, 13, 14, 12, 14, 15,
	    16, 17, 18, 16, 18, 19
	   ]);
	   var buf10 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf10);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf10 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf10);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc10 = gl.getUniformLocation(prog10,"mvMatrix");
	   var prMatLoc10 = gl.getUniformLocation(prog10,"prMatrix");
	   // ****** lines object 11 ******
	   var prog11  = gl.createProgram();
	   gl.attachShader(prog11, getShader( gl, "webgl1vshader11" ));
	   gl.attachShader(prog11, getShader( gl, "webgl1fshader11" ));
	   gl.linkProgram(prog11);
	   var v=new Float32Array([
	    -3.25775, 0.5646551, 0,
	    -3.25775, 0.5646551, 5,
	    -3.25775, 0.5646551, 0,
	    -3.387789, -0.6925371, 0,
	    -3.25775, 0.5646551, 1,
	    -3.387789, -0.6925371, 1,
	    -3.25775, 0.5646551, 2,
	    -3.387789, -0.6925371, 2,
	    -3.25775, 0.5646551, 3,
	    -3.387789, -0.6925371, 3,
	    -3.25775, 0.5646551, 4,
	    -3.387789, -0.6925371, 4,
	    -3.25775, 0.5646551, 5,
	    -3.387789, -0.6925371, 5
	   ]);
	   var posLoc11 = gl.getAttribLocation(prog11, "aPos");
	   var colLoc11 = gl.getAttribLocation(prog11, "aCol");
	   var buf11 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf11);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var mvMatLoc11 = gl.getUniformLocation(prog11,"mvMatrix");
	   var prMatLoc11 = gl.getUniformLocation(prog11,"prMatrix");
	   // ****** text object 12 ******
	   var prog12  = gl.createProgram();
	   gl.attachShader(prog12, getShader( gl, "webgl1vshader12" ));
	   gl.attachShader(prog12, getShader( gl, "webgl1fshader12" ));
	   gl.linkProgram(prog12);
	   var texts = [
	    "0",
	    "1",
	    "2",
	    "3",
	    "4",
	    "5"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX12 = texinfo.canvasX;
	   var canvasY12 = texinfo.canvasY;
	   var ofsLoc12 = gl.getAttribLocation(prog12, "aOfs");
	   var texture12 = gl.createTexture();
	   var texLoc12 = gl.getAttribLocation(prog12, "aTexcoord");
	   var sampler12 = gl.getUniformLocation(prog12,"uSampler");
    	   handleLoadedTexture(texture12, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -3.647865, -3.206922, 0, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 0, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 0, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 0, 0, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 1, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 1, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 1, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 1, 0, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 2, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 2, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 2, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 2, 0, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 3, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 3, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 3, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 3, 0, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 4, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 4, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 4, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 4, 0, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 5, 0, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 5, 1, -0.5, 0.5, 0.5,
	    -3.647865, -3.206922, 5, 1, 1.5, 0.5, 0.5,
	    -3.647865, -3.206922, 5, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<6; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc12 = gl.getAttribLocation(prog12, "aPos");
	   var colLoc12 = gl.getAttribLocation(prog12, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3,
	    4, 5, 6, 4, 6, 7,
	    8, 9, 10, 8, 10, 11,
	    12, 13, 14, 12, 14, 15,
	    16, 17, 18, 16, 18, 19,
	    20, 21, 22, 20, 22, 23
	   ]);
	   var buf12 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf12);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf12 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf12);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc12 = gl.getUniformLocation(prog12,"mvMatrix");
	   var prMatLoc12 = gl.getUniformLocation(prog12,"prMatrix");
	   // ****** text object 13 ******
	   var prog13  = gl.createProgram();
	   gl.attachShader(prog13, getShader( gl, "webgl1vshader13" ));
	   gl.attachShader(prog13, getShader( gl, "webgl1fshader13" ));
	   gl.linkProgram(prog13);
	   var texts = [
	    "Horizontal Axis"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX13 = texinfo.canvasX;
	   var canvasY13 = texinfo.canvasY;
	   var ofsLoc13 = gl.getAttribLocation(prog13, "aOfs");
	   var texture13 = gl.createTexture();
	   var texLoc13 = gl.getAttribLocation(prog13, "aTexcoord");
	   var sampler13 = gl.getUniformLocation(prog13,"uSampler");
    	   handleLoadedTexture(texture13, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -0.6569854, -6.978498, -1.864946, 0, -0.5, 0.5, 0.5,
	    -0.6569854, -6.978498, -1.864946, 1, -0.5, 0.5, 0.5,
	    -0.6569854, -6.978498, -1.864946, 1, 1.5, 0.5, 0.5,
	    -0.6569854, -6.978498, -1.864946, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<1; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc13 = gl.getAttribLocation(prog13, "aPos");
	   var colLoc13 = gl.getAttribLocation(prog13, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3
	   ]);
	   var buf13 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf13);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf13 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf13);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc13 = gl.getUniformLocation(prog13,"mvMatrix");
	   var prMatLoc13 = gl.getUniformLocation(prog13,"prMatrix");
	   // ****** text object 14 ******
	   var prog14  = gl.createProgram();
	   gl.attachShader(prog14, getShader( gl, "webgl1vshader14" ));
	   gl.attachShader(prog14, getShader( gl, "webgl1fshader14" ));
	   gl.linkProgram(prog14);
	   var texts = [
	    "Distance from Home Plate"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX14 = texinfo.canvasX;
	   var canvasY14 = texinfo.canvasY;
	   var ofsLoc14 = gl.getAttribLocation(prog14, "aOfs");
	   var texture14 = gl.createTexture();
	   var texLoc14 = gl.getAttribLocation(prog14, "aTexcoord");
	   var sampler14 = gl.getUniformLocation(prog14,"uSampler");
    	   handleLoadedTexture(texture14, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -4.03798, 25.7085, -1.864946, 0, -0.5, 0.5, 0.5,
	    -4.03798, 25.7085, -1.864946, 1, -0.5, 0.5, 0.5,
	    -4.03798, 25.7085, -1.864946, 1, 1.5, 0.5, 0.5,
	    -4.03798, 25.7085, -1.864946, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<1; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc14 = gl.getAttribLocation(prog14, "aPos");
	   var colLoc14 = gl.getAttribLocation(prog14, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3
	   ]);
	   var buf14 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf14);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf14 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf14);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc14 = gl.getUniformLocation(prog14,"mvMatrix");
	   var prMatLoc14 = gl.getUniformLocation(prog14,"prMatrix");
	   // ****** text object 15 ******
	   var prog15  = gl.createProgram();
	   gl.attachShader(prog15, getShader( gl, "webgl1vshader15" ));
	   gl.attachShader(prog15, getShader( gl, "webgl1fshader15" ));
	   gl.linkProgram(prog15);
	   var texts = [
	    "Height From Ground"
	   ];
	   var texinfo = drawTextToCanvas(texts, 1);	 
	   var canvasX15 = texinfo.canvasX;
	   var canvasY15 = texinfo.canvasY;
	   var ofsLoc15 = gl.getAttribLocation(prog15, "aOfs");
	   var texture15 = gl.createTexture();
	   var texLoc15 = gl.getAttribLocation(prog15, "aTexcoord");
	   var sampler15 = gl.getUniformLocation(prog15,"uSampler");
    	   handleLoadedTexture(texture15, document.getElementById("webgl1textureCanvas"));
	   var v=new Float32Array([
	    -4.03798, -6.978498, 2.352386, 0, -0.5, 0.5, 0.5,
	    -4.03798, -6.978498, 2.352386, 1, -0.5, 0.5, 0.5,
	    -4.03798, -6.978498, 2.352386, 1, 1.5, 0.5, 0.5,
	    -4.03798, -6.978498, 2.352386, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<1; i++) 
	     for (var j=0; j<4; j++) {
	         ind = 7*(4*i + j) + 3;
	         v[ind+2] = 2*(v[ind]-v[ind+2])*texinfo.widths[i]/width;
	         v[ind+3] = 2*(v[ind+1]-v[ind+3])*texinfo.textHeight/height;
	         v[ind] *= texinfo.widths[i]/texinfo.canvasX;
	         v[ind+1] = 1.0-(texinfo.offset + i*texinfo.skip 
	           - v[ind+1]*texinfo.textHeight)/texinfo.canvasY;
	     }
	   var posLoc15 = gl.getAttribLocation(prog15, "aPos");
	   var colLoc15 = gl.getAttribLocation(prog15, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3
	   ]);
	   var buf15 = gl.createBuffer();
	   gl.bindBuffer(gl.ARRAY_BUFFER, buf15);
	   gl.bufferData(gl.ARRAY_BUFFER, v, gl.STATIC_DRAW);
	   var ibuf15 = gl.createBuffer();
	   gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf15);
	   gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.STATIC_DRAW);
	   var mvMatLoc15 = gl.getUniformLocation(prog15,"mvMatrix");
	   var prMatLoc15 = gl.getUniformLocation(prog15,"prMatrix");
	   gl.enable(gl.DEPTH_TEST);
	   gl.depthFunc(gl.LEQUAL);
	   gl.clearDepth(1.0);
	   gl.clearColor(1, 1, 1, 1);
	   var xOffs = yOffs = 0,  drag  = 0;
	   drawScene();
	   function drawScene(){
	     gl.depthMask(true);
	     gl.disable(gl.BLEND);
	     var radius = 27.21732;
	     var s = sin(fov*PI/360);
	     var t = tan(fov*PI/360);
	     var distance = radius/s;
	     var near = distance - radius;
	     var far = distance + radius;
	     var hlen = t*near;
	     var aspect = width/height;
	     prMatrix.makeIdentity();
	     if (aspect > 1)
	       prMatrix.frustum(-hlen*aspect*zoom, hlen*aspect*zoom, 
	                        -hlen*zoom, hlen*zoom, near, far);
	     else  
	       prMatrix.frustum(-hlen*zoom, hlen*zoom, 
	                        -hlen*zoom/aspect, hlen*zoom/aspect, 
	                        near, far);
	     mvMatrix.makeIdentity();
	     mvMatrix.translate( 0.6569854, -25.7085, -2.352386 );
	     mvMatrix.scale( 1, 1, 1 );   
	     mvMatrix.multRight( userMatrix );  
	     mvMatrix.translate(0, 0, -distance);
	     normMatrix.makeIdentity();
	     normMatrix.scale( 1, 1, 1 );   
	     normMatrix.multRight( userMatrix );
	     gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
	     // ****** spheres object 6 *******
	     gl.useProgram(prog6);
	     gl.bindBuffer(gl.ARRAY_BUFFER, sphereBuf);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, sphereIbuf);
	     gl.uniformMatrix4fv( prMatLoc6, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc6, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( normMatLoc6, false, new Float32Array(normMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc6 );
	     gl.vertexAttribPointer(posLoc6,  3, gl.FLOAT, false, 12,  0);
	     gl.enableVertexAttribArray(normLoc6 );
	     gl.vertexAttribPointer(normLoc6,  3, gl.FLOAT, false, 12,  0);
	     gl.disableVertexAttribArray( colLoc6 );
	     var sphereNorm = new CanvasMatrix4();
	     sphereNorm.scale(1, 1, 1);
	     sphereNorm.multRight(normMatrix);
	     gl.uniformMatrix4fv( normMatLoc6, false, new Float32Array(sphereNorm.getAsArray()) );
	     for (var i = 0; i < 387; i++) {
	       var sphereMV = new CanvasMatrix4();
	       var baseofs = i*8
	       var ofs = baseofs + 7;	       
	       var scale = values6[ofs];
	       sphereMV.scale(1*scale, 1*scale, 1*scale);
	       sphereMV.translate(values6[baseofs], 
	       			  values6[baseofs+1], 
	       			  values6[baseofs+2]);
	       sphereMV.multRight(mvMatrix);
	       gl.uniformMatrix4fv( mvMatLoc6, false, new Float32Array(sphereMV.getAsArray()) );
	       ofs = baseofs + 3;       
	       gl.vertexAttrib4f( colLoc6, values6[ofs], 
	       				    values6[ofs+1], 
	       				    values6[ofs+2],
	       				    values6[ofs+3] );
	       gl.drawElements(gl.TRIANGLES, 384, gl.UNSIGNED_SHORT, 0);
	     }
	     // ****** lines object 7 *******
	     gl.useProgram(prog7);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf7);
	     gl.uniformMatrix4fv( prMatLoc7, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc7, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc7 );
	     gl.disableVertexAttribArray( colLoc7 );
	     gl.vertexAttrib4f( colLoc7, 0, 0, 0, 1 );
	     gl.lineWidth( 1 );
	     gl.vertexAttribPointer(posLoc7,  3, gl.FLOAT, false, 12,  0);
	     gl.drawArrays(gl.LINES, 0, 12);
	     // ****** text object 8 *******
	     gl.useProgram(prog8);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf8);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf8);
	     gl.uniformMatrix4fv( prMatLoc8, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc8, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc8 );
	     gl.disableVertexAttribArray( colLoc8 );
	     gl.vertexAttrib4f( colLoc8, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc8 );
	     gl.vertexAttribPointer(texLoc8, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture8);
	     gl.uniform1i( sampler8, 0);
	     gl.enableVertexAttribArray( ofsLoc8 );
	     gl.vertexAttribPointer(ofsLoc8, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc8,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 30, gl.UNSIGNED_SHORT, 0);
	     // ****** lines object 9 *******
	     gl.useProgram(prog9);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf9);
	     gl.uniformMatrix4fv( prMatLoc9, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc9, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc9 );
	     gl.disableVertexAttribArray( colLoc9 );
	     gl.vertexAttrib4f( colLoc9, 0, 0, 0, 1 );
	     gl.lineWidth( 1 );
	     gl.vertexAttribPointer(posLoc9,  3, gl.FLOAT, false, 12,  0);
	     gl.drawArrays(gl.LINES, 0, 12);
	     // ****** text object 10 *******
	     gl.useProgram(prog10);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf10);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf10);
	     gl.uniformMatrix4fv( prMatLoc10, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc10, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc10 );
	     gl.disableVertexAttribArray( colLoc10 );
	     gl.vertexAttrib4f( colLoc10, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc10 );
	     gl.vertexAttribPointer(texLoc10, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture10);
	     gl.uniform1i( sampler10, 0);
	     gl.enableVertexAttribArray( ofsLoc10 );
	     gl.vertexAttribPointer(ofsLoc10, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc10,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 30, gl.UNSIGNED_SHORT, 0);
	     // ****** lines object 11 *******
	     gl.useProgram(prog11);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf11);
	     gl.uniformMatrix4fv( prMatLoc11, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc11, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc11 );
	     gl.disableVertexAttribArray( colLoc11 );
	     gl.vertexAttrib4f( colLoc11, 0, 0, 0, 1 );
	     gl.lineWidth( 1 );
	     gl.vertexAttribPointer(posLoc11,  3, gl.FLOAT, false, 12,  0);
	     gl.drawArrays(gl.LINES, 0, 14);
	     // ****** text object 12 *******
	     gl.useProgram(prog12);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf12);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf12);
	     gl.uniformMatrix4fv( prMatLoc12, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc12, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc12 );
	     gl.disableVertexAttribArray( colLoc12 );
	     gl.vertexAttrib4f( colLoc12, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc12 );
	     gl.vertexAttribPointer(texLoc12, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture12);
	     gl.uniform1i( sampler12, 0);
	     gl.enableVertexAttribArray( ofsLoc12 );
	     gl.vertexAttribPointer(ofsLoc12, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc12,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 36, gl.UNSIGNED_SHORT, 0);
	     // ****** text object 13 *******
	     gl.useProgram(prog13);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf13);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf13);
	     gl.uniformMatrix4fv( prMatLoc13, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc13, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc13 );
	     gl.disableVertexAttribArray( colLoc13 );
	     gl.vertexAttrib4f( colLoc13, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc13 );
	     gl.vertexAttribPointer(texLoc13, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture13);
	     gl.uniform1i( sampler13, 0);
	     gl.enableVertexAttribArray( ofsLoc13 );
	     gl.vertexAttribPointer(ofsLoc13, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc13,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
	     // ****** text object 14 *******
	     gl.useProgram(prog14);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf14);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf14);
	     gl.uniformMatrix4fv( prMatLoc14, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc14, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc14 );
	     gl.disableVertexAttribArray( colLoc14 );
	     gl.vertexAttrib4f( colLoc14, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc14 );
	     gl.vertexAttribPointer(texLoc14, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture14);
	     gl.uniform1i( sampler14, 0);
	     gl.enableVertexAttribArray( ofsLoc14 );
	     gl.vertexAttribPointer(ofsLoc14, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc14,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
	     // ****** text object 15 *******
	     gl.useProgram(prog15);
	     gl.bindBuffer(gl.ARRAY_BUFFER, buf15);
	     gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibuf15);
	     gl.uniformMatrix4fv( prMatLoc15, false, new Float32Array(prMatrix.getAsArray()) );
	     gl.uniformMatrix4fv( mvMatLoc15, false, new Float32Array(mvMatrix.getAsArray()) );
	     gl.enableVertexAttribArray( posLoc15 );
	     gl.disableVertexAttribArray( colLoc15 );
	     gl.vertexAttrib4f( colLoc15, 0, 0, 0, 1 );
	     gl.enableVertexAttribArray( texLoc15 );
	     gl.vertexAttribPointer(texLoc15, 2, gl.FLOAT, false, 28, 12);
	     gl.activeTexture(gl.TEXTURE0);
	     gl.bindTexture(gl.TEXTURE_2D, texture15);
	     gl.uniform1i( sampler15, 0);
	     gl.enableVertexAttribArray( ofsLoc15 );
	     gl.vertexAttribPointer(ofsLoc15, 2, gl.FLOAT, false, 28, 20);
	     gl.vertexAttribPointer(posLoc15,  3, gl.FLOAT, false, 28,  0);
	     gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
	     gl.flush ();
	   }
	   var vlen = function(v) {
	     return sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2])
	   }
	   var xprod = function(a, b) {
	     return [a[1]*b[2] - a[2]*b[1],
	             a[2]*b[0] - a[0]*b[2],
	             a[0]*b[1] - a[1]*b[0]];
	   }
	   var screenToVector = function(x, y) {
	     var radius = max(width, height)/2.0;
	     var cx = width/2.0;
	     var cy = height/2.0;
	     var px = (x-cx)/radius;
	     var py = (y-cy)/radius;
	     var plen = sqrt(px*px+py*py);
	     if (plen > 1.e-6) { 
	       px = px/plen;
	       py = py/plen;
	     }
	     var angle = (SQRT2 - plen)/SQRT2*PI/2;
	     var z = sin(angle);
	     var zlen = sqrt(1.0 - z*z);
	     px = px * zlen;
	     py = py * zlen;
	     return [px, py, z];
	   }
	   var rotBase;
	   var trackballdown = function(x,y) {
	     rotBase = screenToVector(x, y);
	     saveMat.load(userMatrix);
	   }
	   var trackballmove = function(x,y) {
	     var rotCurrent = screenToVector(x,y);
	     var dot = rotBase[0]*rotCurrent[0] + 
	   	       rotBase[1]*rotCurrent[1] + 
	   	       rotBase[2]*rotCurrent[2];
	     var angle = acos( dot/vlen(rotBase)/vlen(rotCurrent) )*180./PI;
	     var axis = xprod(rotBase, rotCurrent);
	     userMatrix.load(saveMat);
	     userMatrix.rotate(angle, axis[0], axis[1], axis[2]);
	     drawScene();
	   }
	   var y0zoom = 0;
	   var zoom0 = 1;
	   var zoomdown = function(x, y) {
	     y0zoom = y;
	     zoom0 = log(zoom);
	   }
	   var zoommove = function(x, y) {
	     zoom = exp(zoom0 + (y-y0zoom)/height);
	     drawScene();
	   }
	   var y0fov = 0;
	   var fov0 = 1;
	   var fovdown = function(x, y) {
	     y0fov = y;
	     fov0 = fov;
	   }
	   var fovmove = function(x, y) {
	     fov = max(1, min(179, fov0 + 180*(y-y0fov)/height));
	     drawScene();
	   }
	   var mousedown = [trackballdown, zoomdown, fovdown];
	   var mousemove = [trackballmove, zoommove, fovmove];
	   function relMouseCoords(event){
	     var totalOffsetX = 0;
	     var totalOffsetY = 0;
	     var currentElement = canvas;
	     do{
	       totalOffsetX += currentElement.offsetLeft;
	       totalOffsetY += currentElement.offsetTop;
	     }
	     while(currentElement = currentElement.offsetParent)
	     var canvasX = event.pageX - totalOffsetX;
	     var canvasY = event.pageY - totalOffsetY;
	     return {x:canvasX, y:canvasY}
	   }
	   canvas.onmousedown = function ( ev ){
	     if (!ev.which) // Use w3c defns in preference to MS
	       switch (ev.button) {
	       case 0: ev.which = 1; break;
	       case 1: 
	       case 4: ev.which = 2; break;
	       case 2: ev.which = 3;
	     }
	     drag = ev.which;
	     var f = mousedown[drag-1];
	     if (f) {
	       var coords = relMouseCoords(ev);
	       f(coords.x, height-coords.y); 
	       ev.preventDefault();
	     }
	   }    
	   canvas.onmouseup = function ( ev ){	
	     drag = 0;
	   }
	   canvas.onmouseout = canvas.onmouseup;
	   canvas.onmousemove = function ( ev ){
	     if ( drag == 0 ) return;
	     var f = mousemove[drag-1];
	     if (f) {
	       var coords = relMouseCoords(ev);
	       f(coords.x, height-coords.y);
	     }
	   }
	   var wheelHandler = function(ev) {
	     var del = 1.1;
	     if (ev.shiftKey) del = 1.01;
	     var ds = ((ev.detail || ev.wheelDelta) > 0) ? del : (1 / del);
	     zoom *= ds;
	     drawScene();
	     ev.preventDefault();
	   };
	   canvas.addEventListener("DOMMouseScroll", wheelHandler, false);
	   canvas.addEventListener("mousewheel", wheelHandler, false);
	}
</script>
<canvas id="webgl1canvas" width="1" height="1"></canvas> 
<p id="webgl1debug">
<img src="webgl1snapshot.png" alt="webgl1snapshot" width=1441/><br>
	You must enable Javascript to view this page properly.</p>
<script>webgl1webGLStart();</script>



#### Authored by <a href="http://cpsievert.github.com/home.html">Carson Sievert</a> via <a href="http://www.rstudio.com/ide/docs/authoring/using_markdown">R Markdown, knitr & RStudio</a>. Published by <a href="http://pages.github.com/">GitHub Pages</a>.
