





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
	    -2.127667, 55, 5.445333, 0, 0.6509804, 0, 1, 0.12,
	    -2.631333, 55, 5.074, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.354071, 55, 5.214095, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -2.3734, 55, 5.1028, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.750049, 55, 5.177, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.094555, 54.08084, 5.497062, 0, 0.6509804, 0, 1, 0.12,
	    -2.561087, 53.67742, 5.033531, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.265079, 53.62394, 5.161435, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -2.285935, 53.65596, 5.05076, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.693956, 53.78031, 5.161239, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.060761, 53.16345, 5.544666, 0, 0.6509804, 0, 1, 0.12,
	    -2.490683, 52.35765, 4.991231, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.176727, 52.25117, 5.107705, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -2.199996, 52.31498, 4.996895, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.636444, 52.56328, 5.142352, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -2.026285, 52.24781, 5.588147, 0, 0.6509804, 0, 1, 0.12,
	    -2.42012, 51.0407, 4.9471, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.089013, 50.88171, 5.052905, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -2.115584, 50.97706, 4.941206, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.577513, 51.34889, 5.120339, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.991126, 51.33393, 5.627504, 0, 0.6509804, 0, 1, 0.12,
	    -2.349398, 49.72656, 4.901138, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -2.001939, 49.51554, 4.997036, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -2.032698, 49.64221, 4.883692, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.517163, 50.13716, 5.095201, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.955285, 50.42182, 5.662737, 0, 0.6509804, 0, 1, 0.12,
	    -2.278517, 48.41523, 4.853344, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.915503, 48.15268, 4.940098, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.951339, 48.31042, 4.824354, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.455393, 48.92808, 5.066938, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.91876, 49.51146, 5.693847, 0, 0.6509804, 0, 1, 0.12,
	    -2.207478, 47.10673, 4.80372, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.829707, 46.7931, 4.88209, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.871505, 46.9817, 4.763191, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.392204, 47.72165, 5.035549, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.881554, 48.60286, 5.720833, 0, 0.6509804, 0, 1, 0.12,
	    -2.13628, 45.80103, 4.752264, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.744551, 45.43683, 4.823013, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.793198, 45.65603, 4.700204, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.327596, 46.51786, 5.001034, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.843665, 47.69603, 5.743696, 0, 0.6509804, 0, 1, 0.12,
	    -2.064924, 44.49815, 4.698978, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.660033, 44.08385, 4.762866, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.716417, 44.33344, 4.635392, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.261569, 45.31673, 4.963394, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.805093, 46.79095, 5.762435, 0, 0.6509804, 0, 1, 0.12,
	    -1.993409, 43.19809, 4.64386, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.576155, 42.73417, 4.701651, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.641162, 43.0139, 4.568756, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.194123, 44.11826, 4.922628, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.765838, 45.88764, 5.77705, 0, 0.6509804, 0, 1, 0.12,
	    -1.921735, 41.90084, 4.586912, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.492916, 41.38779, 4.639366, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.567434, 41.69743, 4.500295, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.125257, 42.92243, 4.878737, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.725901, 44.98608, 5.787541, 0, 0.6509804, 0, 1, 0.12,
	    -1.849903, 40.6064, 4.528132, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.410316, 40.0447, 4.576011, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.495232, 40.38402, 4.43001, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -2.054972, 41.72925, 4.83172, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.685282, 44.08628, 5.79391, 0, 0.6509804, 0, 1, 0.12,
	    -1.777912, 39.31479, 4.467521, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.328355, 38.70492, 4.511587, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.424556, 39.07367, 4.3579, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.983268, 40.53873, 4.781578, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.64398, 43.18825, 5.796154, 0, 0.6509804, 0, 1, 0.12,
	    -1.705762, 38.02598, 4.405079, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.247034, 37.36843, 4.446093, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.355407, 37.76639, 4.283966, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.910145, 39.35085, 4.72831, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.601995, 42.29197, 5.794274, 0, 0.6509804, 0, 1, 0.12,
	    -1.633454, 36.73999, 4.340806, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.166352, 36.03524, 4.37953, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.287783, 36.46217, 4.208207, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.835602, 38.16563, 4.671916, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.559328, 41.39746, 5.788271, 0, 0.6509804, 0, 1, 0.12,
	    -1.560987, 35.45681, 4.274701, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.086309, 34.70535, 4.311898, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.221686, 35.16102, 4.130624, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.759641, 36.98306, 4.612397, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.515978, 40.5047, 5.778144, 0, 0.6509804, 0, 1, 0.12,
	    -1.488362, 34.17645, 4.206766, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -1.006905, 33.37875, 4.243196, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.157116, 33.86293, 4.051216, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.68226, 35.80313, 4.549753, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.471946, 39.6137, 5.763894, 0, 0.6509804, 0, 1, 0.12,
	    -1.415578, 32.8989, 4.137, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.9281402, 32.05545, 4.173425, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.094072, 32.5679, 3.969984, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.60346, 34.62587, 4.483983, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.427231, 38.72447, 5.745519, 0, 0.6509804, 0, 1, 0.12,
	    -1.342635, 31.62417, 4.065402, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.8500149, 30.73545, 4.102584, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -1.032553, 31.27593, 3.886927, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.523241, 33.45125, 4.415087, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.381833, 37.83699, 5.723022, 0, 0.6509804, 0, 1, 0.12,
	    -1.269533, 30.35225, 3.991973, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.7725288, 29.41875, 4.030674, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.9725615, 29.98703, 3.802046, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.441602, 32.27928, 4.343065, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.335753, 36.95128, 5.6964, 0, 0.6509804, 0, 1, 0.12,
	    -1.196273, 29.08315, 3.916713, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6956819, 28.10534, 3.957695, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.914096, 28.70119, 3.71534, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.358544, 31.10996, 4.267919, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.288991, 36.06733, 5.665655, 0, 0.6509804, 0, 1, 0.12,
	    -1.122855, 27.81687, 3.839622, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.6194742, 26.79524, 3.883646, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.8571568, 27.41842, 3.62681, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.274068, 29.9433, 4.189646, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.241546, 35.18513, 5.630786, 0, 0.6509804, 0, 1, 0.12,
	    -1.049277, 26.55339, 3.7607, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.5439058, 25.48843, 3.808528, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.8017437, 26.1387, 3.536455, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.188171, 28.77928, 4.108248, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.193418, 34.3047, 5.591794, 0, 0.6509804, 0, 1, 0.12,
	    -0.9755415, 25.29274, 3.679947, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.4689765, 24.18492, 3.732341, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.747857, 24.86206, 3.444276, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.100856, 27.61792, 4.023725, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.144607, 33.42602, 5.548677, 0, 0.6509804, 0, 1, 0.12,
	    -0.9016469, 24.03489, 3.597363, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3946865, 22.8847, 3.655084, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.6954966, 23.58847, 3.350272, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -1.012122, 26.4592, 3.936076, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.095115, 32.5491, 5.501438, 0, 0.6509804, 0, 1, 0.12,
	    -0.8275937, 22.77987, 3.512948, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.3210357, 21.58778, 3.576757, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.6446625, 22.31795, 3.254444, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.921968, 25.30314, 3.845301, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -1.044939, 31.67395, 5.450074, 0, 0.6509804, 0, 1, 0.12,
	    -0.7533819, 21.52765, 3.426702, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.2480241, 20.29416, 3.497362, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.5953546, 21.05049, 3.156791, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.8303951, 24.14973, 3.751401, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.9940811, 30.80055, 5.394587, 0, 0.6509804, 0, 1, 0.12,
	    -0.6790115, 20.27825, 3.338624, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1756518, 19.00384, 3.416897, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.5475731, 19.7861, 3.057314, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.737403, 22.99897, 3.654375, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.9425405, 29.92892, 5.334976, 0, 0.6509804, 0, 1, 0.12,
	    -0.6044824, 19.03167, 3.248715, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.1039186, 17.71682, 3.335362, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.5013177, 18.52477, 2.956012, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.6429917, 21.85086, 3.554224, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.8903174, 29.05904, 5.271242, 0, 0.6509804, 0, 1, 0.12,
	    -0.5297947, 17.7879, 3.156976, 0.9019608, 0.9019608, 0, 1, 0.12,
	    -0.03282471, 16.43309, 3.252758, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.4565887, 17.2665, 2.852886, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.5471612, 20.70541, 3.450947, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.8374116, 28.19093, 5.203383, 0, 0.6509804, 0, 1, 0.12,
	    -0.4549483, 16.54695, 3.063405, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.03763, 15.15266, 3.169085, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.413386, 16.01129, 2.747935, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.4499115, 19.5626, 3.344545, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.7838234, 27.32458, 5.131402, 0, 0.6509804, 0, 1, 0.12,
	    -0.3799433, 15.30881, 2.968003, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.1074455, 13.87553, 3.084342, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.3717095, 14.75915, 2.64116, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.3512425, 18.42245, 3.235017, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.7295525, 26.45998, 5.055296, 0, 0.6509804, 0, 1, 0.12,
	    -0.3047797, 14.07348, 2.87077, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.1766217, 12.6017, 2.998529, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.3315594, 13.51007, 2.53256, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.2511544, 17.28494, 3.122363, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.6745991, 25.59715, 4.975067, 0, 0.6509804, 0, 1, 0.12,
	    -0.2294575, 12.84097, 2.771706, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.2451588, 11.33116, 2.911648, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.2929355, 12.26406, 2.422136, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.1496471, 16.15009, 3.006584, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.6189631, 24.73607, 4.890714, 0, 0.6509804, 0, 1, 0.12,
	    -0.1539766, 11.61127, 2.670811, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.3130566, 10.06393, 2.823697, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.2558378, 11.02111, 2.309887, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    -0.04672056, 15.01788, 2.88768, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.5626446, 23.87676, 4.802238, 0, 0.6509804, 0, 1, 0.12,
	    -0.07833708, 10.38439, 2.568084, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.3803152, 8.799986, 2.734677, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.2202665, 9.781218, 2.195814, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.05762518, 13.88833, 2.76565, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.5056435, 23.0192, 4.709637, 0, 0.6509804, 0, 1, 0.12,
	    -0.002538933, 9.160329, 2.463527, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.4469346, 7.539343, 2.644587, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.1862214, 8.544394, 2.079916, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.1633901, 12.76143, 2.640494, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.4479598, 22.16341, 4.612914, 0, 0.6509804, 0, 1, 0.12,
	    0.07341785, 7.939078, 2.357139, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.5129148, 6.281998, 2.553427, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.1537027, 7.310632, 1.962194, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.2705743, 11.63718, 2.512213, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.3895935, 21.30937, 4.512066, 0, 0.6509804, 0, 1, 0.12,
	    0.1495333, 6.720642, 2.248919, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.5782558, 5.02795, 2.461199, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.1227102, 6.079934, 1.842647, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.3791776, 10.51559, 2.380806, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.3305447, 20.4571, 4.407095, 0, 0.6509804, 0, 1, 0.12,
	    0.2258073, 5.50502, 2.138868, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.6429575, 3.777201, 2.367901, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.09324394, 4.852299, 1.721276, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.4892002, 9.396639, 2.246274, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.2708133, 19.60659, 4.298, 0, 0.6509804, 0, 1, 0.12,
	    0.30224, 4.292213, 2.026987, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.70702, 2.529749, 2.273533, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.065304, 3.627728, 1.59808, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.600642, 8.280343, 2.108616, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.2103994, 18.75783, 4.184782, 0, 0.6509804, 0, 1, 0.12,
	    0.3788313, 3.082221, 1.913274, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.03889034, 2.40622, 1.47306, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.7135029, 7.166698, 1.967832, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.1493029, 17.91084, 4.06744, 0, 0.6509804, 0, 1, 0.12,
	    0.4555813, 1.875044, 1.79773, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.8277831, 6.055705, 1.823923, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.08752378, 17.06561, 3.945973, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    0.9434825, 4.947362, 1.676889, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    -0.02506213, 16.22213, 3.820384, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.060601, 3.84167, 1.526728, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.03808208, 15.38042, 3.690671, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.179139, 2.73863, 1.373443, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.1019089, 14.54046, 3.556834, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.299096, 1.63824, 1.217032, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.1664182, 13.70227, 3.418874, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.2316101, 12.86584, 3.276789, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.2974846, 12.03116, 3.130582, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.3640417, 11.19825, 2.98025, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.4312813, 10.3671, 2.825795, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.4992035, 9.537704, 2.667216, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.5678082, 8.710071, 2.504514, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.6370955, 7.884197, 2.337687, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.7070654, 7.060084, 2.166738, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.7777179, 6.237731, 1.991664, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.8490529, 5.417138, 1.812467, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.9210705, 4.598306, 1.629146, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    0.9937706, 3.781233, 1.441702, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.067153, 2.96592, 1.250133, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.141219, 2.152367, 1.054441, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12,
	    1.208893, 1.417, 0.8736318, 0, 0.6509804, 0, 1, 0.12,
	    0.4847913, 1.417, 1.753338, 0.9019608, 0.9019608, 0, 1, 0.12,
	    0.7637669, 1.417, 2.188239, 0.9176471, 0.7137255, 0.3058824, 1, 0.12,
	    -0.01857312, 1.417, 1.370242, 0.9333333, 0.7254902, 0.6235294, 1, 0.12,
	    1.323421, 1.417, 1.185161, 0.9490196, 0.9490196, 0.9490196, 1, 0.12
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
	    -2, 0.4896551, 0.676194,
	    1, 0.4896551, 0.676194,
	    -2, 0.4896551, 0.676194,
	    -2, -0.8962871, 0.5432591,
	    -1, 0.4896551, 0.676194,
	    -1, -0.8962871, 0.5432591,
	    0, 0.4896551, 0.676194,
	    0, -0.8962871, 0.5432591,
	    1, 0.4896551, 0.676194,
	    1, -0.8962871, 0.5432591
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
	    -2, -3.668172, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -2, -3.668172, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -2, -3.668172, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -2, -3.668172, 0.2773892, 0, 1.5, 0.5, 0.5,
	    -1, -3.668172, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -1, -3.668172, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -1, -3.668172, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -1, -3.668172, 0.2773892, 0, 1.5, 0.5, 0.5,
	    0, -3.668172, 0.2773892, 0, -0.5, 0.5, 0.5,
	    0, -3.668172, 0.2773892, 1, -0.5, 0.5, 0.5,
	    0, -3.668172, 0.2773892, 1, 1.5, 0.5, 0.5,
	    0, -3.668172, 0.2773892, 0, 1.5, 0.5, 0.5,
	    1, -3.668172, 0.2773892, 0, -0.5, 0.5, 0.5,
	    1, -3.668172, 0.2773892, 1, -0.5, 0.5, 0.5,
	    1, -3.668172, 0.2773892, 1, 1.5, 0.5, 0.5,
	    1, -3.668172, 0.2773892, 0, 1.5, 0.5, 0.5
	   ]);
	   for (var i=0; i<4; i++) 
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
	    12, 13, 14, 12, 14, 15
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
	    -2.934751, 10, 0.676194,
	    -2.934751, 50, 0.676194,
	    -2.934751, 10, 0.676194,
	    -3.045823, 10, 0.5432591,
	    -2.934751, 20, 0.676194,
	    -3.045823, 20, 0.5432591,
	    -2.934751, 30, 0.676194,
	    -3.045823, 30, 0.5432591,
	    -2.934751, 40, 0.676194,
	    -3.045823, 40, 0.5432591,
	    -2.934751, 50, 0.676194,
	    -3.045823, 50, 0.5432591
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
	    -3.267966, 10, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -3.267966, 10, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -3.267966, 10, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -3.267966, 10, 0.2773892, 0, 1.5, 0.5, 0.5,
	    -3.267966, 20, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -3.267966, 20, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -3.267966, 20, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -3.267966, 20, 0.2773892, 0, 1.5, 0.5, 0.5,
	    -3.267966, 30, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -3.267966, 30, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -3.267966, 30, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -3.267966, 30, 0.2773892, 0, 1.5, 0.5, 0.5,
	    -3.267966, 40, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -3.267966, 40, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -3.267966, 40, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -3.267966, 40, 0.2773892, 0, 1.5, 0.5, 0.5,
	    -3.267966, 50, 0.2773892, 0, -0.5, 0.5, 0.5,
	    -3.267966, 50, 0.2773892, 1, -0.5, 0.5, 0.5,
	    -3.267966, 50, 0.2773892, 1, 1.5, 0.5, 0.5,
	    -3.267966, 50, 0.2773892, 0, 1.5, 0.5, 0.5
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
	    -2.934751, 0.4896551, 1,
	    -2.934751, 0.4896551, 5,
	    -2.934751, 0.4896551, 1,
	    -3.045823, -0.8962871, 1,
	    -2.934751, 0.4896551, 2,
	    -3.045823, -0.8962871, 2,
	    -2.934751, 0.4896551, 3,
	    -3.045823, -0.8962871, 3,
	    -2.934751, 0.4896551, 4,
	    -3.045823, -0.8962871, 4,
	    -2.934751, 0.4896551, 5,
	    -3.045823, -0.8962871, 5
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
	    -3.267966, -3.668172, 1, 0, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 1, 1, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 1, 1, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 1, 0, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 2, 0, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 2, 1, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 2, 1, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 2, 0, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 3, 0, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 3, 1, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 3, 1, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 3, 0, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 4, 0, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 4, 1, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 4, 1, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 4, 0, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 5, 0, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 5, 1, -0.5, 0.5, 0.5,
	    -3.267966, -3.668172, 5, 1, 1.5, 0.5, 0.5,
	    -3.267966, -3.668172, 5, 0, 1.5, 0.5, 0.5
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
	   var posLoc12 = gl.getAttribLocation(prog12, "aPos");
	   var colLoc12 = gl.getAttribLocation(prog12, "aCol");
	   var f=new Uint16Array([
	    0, 1, 2, 0, 2, 3,
	    4, 5, 6, 4, 6, 7,
	    8, 9, 10, 8, 10, 11,
	    12, 13, 14, 12, 14, 15,
	    16, 17, 18, 16, 18, 19
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
	    -0.7133141, -7.825998, -0.1214156, 0, -0.5, 0.5, 0.5,
	    -0.7133141, -7.825998, -0.1214156, 1, -0.5, 0.5, 0.5,
	    -0.7133141, -7.825998, -0.1214156, 1, 1.5, 0.5, 0.5,
	    -0.7133141, -7.825998, -0.1214156, 0, 1.5, 0.5, 0.5
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
	    -3.601182, 28.2085, -0.1214156, 0, -0.5, 0.5, 0.5,
	    -3.601182, 28.2085, -0.1214156, 1, -0.5, 0.5, 0.5,
	    -3.601182, 28.2085, -0.1214156, 1, 1.5, 0.5, 0.5,
	    -3.601182, 28.2085, -0.1214156, 0, 1.5, 0.5, 0.5
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
	    -3.601182, -7.825998, 3.334893, 0, -0.5, 0.5, 0.5,
	    -3.601182, -7.825998, 3.334893, 1, -0.5, 0.5, 0.5,
	    -3.601182, -7.825998, 3.334893, 1, 1.5, 0.5, 0.5,
	    -3.601182, -7.825998, 3.334893, 0, 1.5, 0.5, 0.5
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
	     var radius = 29.83299;
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
	     mvMatrix.translate( 0.7133141, -28.2085, -3.334893 );
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
	     for (var i = 0; i < 315; i++) {
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
	     gl.drawArrays(gl.LINES, 0, 10);
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
	     gl.drawElements(gl.TRIANGLES, 24, gl.UNSIGNED_SHORT, 0);
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
	     gl.drawArrays(gl.LINES, 0, 12);
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
	     gl.drawElements(gl.TRIANGLES, 30, gl.UNSIGNED_SHORT, 0);
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
