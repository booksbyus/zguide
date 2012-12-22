<?php
/*
 * A2S_ASCIIToSVG.php: ASCII diagram -> SVG art generator.
 * Copyright © 2012 Devon H. O'Dell <devon.odell@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  o Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  o Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#namespace org\dh0\a2s;

include 'svg-path.lex.php';
include 'colors.php';

/*
 * A2S_Scale is a singleton class that is instantiated to apply scale
 * transformations on the text -> canvas grid geometry. We could probably use
 * SVG's native scaling for this, but I'm not sure how yet.
 */
class A2S_Scale {
  private static $instance = null;

  public $xScale;
  public $yScale;

  private function __construct() {}
  private function __clone() {}

  public static function getInstance() {
    if (self::$instance == null) {
      self::$instance = new A2S_Scale();
    }

    return self::$instance;
  }

  public function setScale($x, $y) {
    $o = self::getInstance();
    $o->xScale = $x;
    $o->yScale = $y;
  }
}

/*
 * A2S_CustomObjects allows users to create their own custom SVG paths and use
 * them as box types with a2s:type references.
 *
 * Paths must have width and height set, and must not span multiple lines.
 * Multiple paths can be specified, one path per line. All objects must
 * reside in the same directory.
 *
 * File operations are horribly slow, so we make a best effort to avoid
 * as many as possible:
 *
 *  * If the directory mtime hasn't changed, we attempt to load our
 *    objects from a cache file.
 *
 *  * If this file doesn't exist, can't be read, or the mtime has
 *    changed, we scan the directory and update files that have changed
 *    based on their mtime.
 *
 *  * We attempt to save our cache in a temporary directory. It's volatile
 *    but also requires no configuration.
 *
 * We could do a bit better by utilizing APC's shared memory storage, which
 * would help greatly when running on a server.
 *
 * Note that the path parser isn't foolproof, mostly because PHP isn't the
 * greatest language ever for implementing a parser.
 */
class A2S_CustomObjects {
  public static $objects = array();

  /*
   * Closures / callable function names / whatever for integrating non-default
   * loading and storage functionality.
   */
  public static $loadCacheFn = null;
  public static $storCacheFn = null;
  public static $loadObjsFn = null;


  public static function loadObjects() {
    $cacheFile = sys_get_temp_dir() . "/.a2s.objcache";
    $dir = './objects';

    if (is_callable(self::$loadCacheFn)) {
      /*
       * Should return exactly what was given to the $storCacheFn when it was
       * last called, or null if nothing can be loaded.
       */
      $fn = self::$loadCacheFn;
      self::$objects = $fn();
      return;
    } else {
      if (is_readable($cacheFile) && is_readable($dir)) {
        $cacheTime = filemtime($cacheFile);

        if (filemtime($dir) <= filemtime($cacheFile)) {
          self::$objects = unserialize(file_get_contents($cacheFile));
          return;
        }
      } else {
        $cacheTime = 0;
      }
    }

    if (is_callable(self::$loadObjsFn)) {
      /*
       * Returns an array of arrays of path information. The innermost arrays
       * (containing the path information) contain the path name, the width of
       * the bounding box, the height of the bounding box, and the path
       * command. This interface does *not* want the path's XML tag. An array
       * returned from here containing two objects that each have 1 line would
       * look like:
       *
       * array (
       *   array (
       *     name => 'pathA',
       *     paths => array (
       *       array ('width' => 10, 'height' => 10, 'path' => 'M 0 0 L 10 10'),
       *       array ('width' => 10, 'height' => 10, 'path' => 'M 0 10 L 10 0'),
       *     ),
       *   ),
       *   array (
       *     name => 'pathB',
       *     paths => array (
       *       array ('width' => 10, 'height' => 10, 'path' => 'M 0 5 L 5 10'),
       *       array ('width' => 10, 'height' => 10, 'path' => 'M 5 10 L 10 5'),
       *     ),
       *   ),
       * );
       */
      $fn = self::$loadObjsFn;
      $objs = $fn();

      $i = 0;
      foreach ($objs as $obj) {
        foreach ($obj['paths'] as $path) {
          self::$objects[$obj['name']][$i]['width'] = $path['width'];
          self::$objects[$obj['name']][$i]['height'] = $path['height'];
          self::$objects[$obj['name']][$i++]['path'] =
            self::parsePath($path['path']);
        }
      }
    } else {
      $ents = scandir($dir);
      foreach ($ents as $ent) {
        $file = "{$dir}/{$ent}";
        $base = substr($ent, 0, -5);
        if (substr($ent, -5) == '.path' && is_readable($file)) {
          if (isset(self::$objects[$base]) &&
              filemtime($file) <= self::$cacheTime) {
            continue;
          }

          $lines = file($file);

          $i = 0;
          foreach ($lines as $line) {
            preg_match('/width="(\d+)/', $line, $m);
            $width = $m[1];
            preg_match('/height="(\d+)/', $line, $m);
            $height = $m[1];
            preg_match('/d="([^"]+)"/', $line, $m);
            $path = $m[1];

            self::$objects[$base][$i]['width'] = $width;
            self::$objects[$base][$i]['height'] = $height;
            self::$objects[$base][$i++]['path'] = self::parsePath($path);
          }
        }
      }
    }

    if (is_callable(self::$storCacheFn)) {
      $fn = self::$storCacheFn;
      $fn(self::$objects);
    } else {
      file_put_contents($cacheFile, serialize(self::$objects));
    }
  }

  private static function parsePath($path) {
    $stream = fopen("data://text/plain,{$path}", 'r');

    $P = new A2S_SVGPathParser();
    $S = new A2S_Yylex($stream);

    while ($t = $S->nextToken()) {
      $P->A2S_SVGPath($t->type, $t);
    }
    /* Force shift/reduce of last token. */
    $P->A2S_SVGPath(0);

    fclose($stream);

    $cmdArr = array();
    $i = 0;
    foreach ($P->commands as $cmd) {
      foreach ($cmd as $arg) {
        $arg = (array)$arg;
        $cmdArr[$i][] = $arg['value'];
      }
      $i++;
    }

    return $cmdArr;
  }
}

/*
 * All lines and polygons are represented as a series of point coordinates
 * along a path. Points can have different properties; markers appear on
 * edges of lines and control points denote that a bezier curve should be
 * calculated for the corner represented by this point.
 */
class A2S_Point {
  public $gridX;
  public $gridY;

  public $x;
  public $y;

  public $flags;

  const POINT    = 0x1;
  const CONTROL  = 0x2;
  const SMARKER  = 0x4;
  const IMARKER  = 0x8;
  const TICK     = 0x10;
  const DOT      = 0x20;

  public function __construct($x, $y) {
    $this->flags = 0;

    $s = A2S_Scale::getInstance();
    $this->x = ($x * $s->xScale) + ($s->xScale / 2);
    $this->y = ($y * $s->yScale) + ($s->yScale / 2);

    $this->gridX = $x;
    $this->gridY = $y;
  }
}

/*
 * Groups objects together and sets common properties for the objects in the
 * group.
 */
class A2S_SVGGroup {
  private $groups;
  private $curGroup;
  private $groupStack;
  private $options;

  public function __construct() {
    $this->groups = array();
    $this->groupStack = array();
    $this->options = array();
  }

  public function getGroup($groupName) {
    return $this->groups[$groupName];
  }

  public function pushGroup($groupName) {
    if (!isset($this->groups[$groupName])) {
      $this->groups[$groupName] = array();
      $this->options[$groupName] = array();
    }

    $this->groupStack[] = $groupName;
    $this->curGroup = $groupName;
  }

  public function popGroup() {
    /*
     * Remove the last group and fetch the current one. array_pop will return
     * NULL for an empty array, so this is safe to do when only one element
     * is left.
     */
    array_pop($this->groupStack);
    $this->curGroup = array_pop($this->groupStack);
  }

  public function addObject($o) {
    $this->groups[$this->curGroup][] = $o;
  }

  public function setOption($opt, $val) {
    $this->options[$this->curGroup][$opt] = $val;
  }

  public function render() {
    $out = '';

    foreach($this->groups as $groupName => $objects) {
      $out .= "<g id=\"{$groupName}\" ";
      foreach ($this->options[$groupName] as $opt => $val) {
        if (strpos($opt, 'a2s:', 0) === 0) {
          continue;
        }
        $out .= "$opt=\"$val\" ";
      }
      $out .= ">\n";

      foreach($objects as $obj) {
        $out .= $obj->render();
      }

      $out .= "</g>\n";
    }

    return $out;
  }
}

/*
 * The Path class represents lines and polygons.
 */
class A2S_SVGPath {
  private $options;
  private $points;
  private $ticks;
  private $flags;
  private $text;
  private $name;

  private static $id = 0;

  const CLOSED = 0x1;

  public function __construct() {
    $this->options = array();
    $this->points = array();
    $this->text = array();
    $this->ticks = array();
    $this->flags = 0;
    $this->name = self::$id++;
  }

  /*
   * Making sure that we always started at the top left coordinate 
   * makes so many things so much easier. First, find the lowest Y
   * position. Then, of all matching Y positions, find the lowest X
   * position. This is the top left.
   *
   * As far as the points are considered, they're definitely on the
   * top somewhere, but not necessarily the most left. This could
   * happen if there was a corner connector in the top edge (perhaps
   * for a line to connect to). Since we couldn't turn right there,
   * we have to try now.
   *
   * This should only be called when we close a polygon.
   */
  public function orderPoints() {
    $pPoints = count($this->points);

    $minY = $this->points[0]->y;
    $minX = $this->points[0]->x;
    $minIdx = 0;
    for ($i = 1; $i < $pPoints; $i++) {
      if ($this->points[$i]->y <= $minY) {
        $minY = $this->points[$i]->y;

        if ($this->points[$i]->x < $minX) {
          $minX = $this->points[$i]->x;
          $minIdx = $i;
        }
      }
    }

    /*
     * If our top left isn't at the 0th index, it is at the end. If
     * there are bits after it, we need to cut those and put them at
     * the front.
     */
    if ($minIdx != 0) {
      $startPoints = array_splice($this->points, $minIdx);
      $this->points = array_merge($startPoints, $this->points);
    }
  }

  /*
   * Useful for recursive walkers when speculatively trying a direction.
   */
  public function popPoint() {
    array_pop($this->points);
  }

  public function addPoint($x, $y, $flags = A2S_Point::POINT) {
    $p = new A2S_Point($x, $y);

    /*
     * If we attempt to add our original point back to the path, the polygon
     * must be closed.
     */
    if (count($this->points) > 0) {
      if ($this->points[0]->x == $p->x && $this->points[0]->y == $p->y) {
        $this->flags |= self::CLOSED;
        return true;
      }

      /*
      * For the purposes of this library, paths should never intersect each
      * other. Even in the case of closing the polygon, we do not store the
      * final coordinate twice.
      */
      foreach ($this->points as $point) {
        if ($point->x == $p->x && $point->y == $p->y) {
          return true;
        }
      }
    }

    $p->flags |= $flags;
    $this->points[] = $p;

    return false;
  }

  /*
   * It's useful to be able to know the points in a shape.
   */
  public function getPoints() {
    return $this->points;
  }

  /*
   * Add a marker to a line. The third argument specifies which marker to use,
   * and this depends on the orientation of the line. Due to the way the line
   * parser works, we may have to use an inverted representation.
   */
  public function addMarker($x, $y, $t) {
    $p = new A2S_Point($x, $y);
    $p->flags |= $t;
    $this->points[] = $p;
  }

  public function addTick($x, $y, $t) {
    $p = new A2S_Point($x, $y);
    $p->flags |= $t;
    $this->ticks[] = $p;
  }

  /*
   * Is this path closed?
   */
  public function isClosed() {
    return ($this->flags & self::CLOSED);
  }

  public function addText($t) {
    $this->text[] = $t;
  }

  public function getText() {
    return $this->text;
  }

  public function setID($id) {
    $this->name = str_replace(' ', '_', str_replace('"', '_', $id));
  }

  public function getID() {
    return $this->name;
  }

  /*
   * Set options as a JSON string. Specified as a merge operation so that it
   * can be called after an individual setOption call.
   */
  public function setOptions($opt) {
    $this->options = array_merge($this->options, $opt);
  }

  public function setOption($opt, $val) {
    $this->options[$opt] = $val;
  }

  public function getOption($opt) {
    if (isset($this->options[$opt])) {
      return $this->options[$opt];
    }

    return null;
  }

  /*
   * Does the given point exist within this polygon? Since we can
   * theoretically have some complex concave and convex polygon edges in the
   * same shape, we need to do a full point-in-polygon test. This algorithm
   * seems like the standard one. See: http://alienryderflex.com/polygon/
   */
  public function hasPoint($x, $y) {
    if ($this->isClosed() == false) {
      return false;
    }

    $oddNodes = false;

    $bound = count($this->points);
    for ($i = 0, $j = count($this->points) - 1; $i < $bound; $i++) {
      if (($this->points[$i]->gridY < $y && $this->points[$j]->gridY >= $y ||
           $this->points[$j]->gridY < $y && $this->points[$i]->gridY >= $y) &&
          ($this->points[$i]->gridX <= $x || $this->points[$j]->gridX <= $x)) {
        if ($this->points[$i]->gridX + ($y - $this->points[$i]->gridY) /
            ($this->points[$j]->gridY - $this->points[$i]->gridY) *
            ($this->points[$j]->gridX - $this->points[$i]->gridX) < $x) {
          $oddNodes = !$oddNodes;
        }
      }

      $j = $i;
    }

    return $oddNodes;
  }

  /* 
   * Apply a matrix transformation to the coordinates ($x, $y). The
   * multiplication is implemented on the matrices:
   *
   * | a b c |   | x |
   * | d e f | * | y |
   * | 0 0 1 |   | 1 |
   *
   * Additional information on the transformations and what each R,C in the
   * transformation matrix represents, see:
   *
   * http://www.w3.org/TR/SVG/coords.html#TransformMatrixDefined
   */
  private function matrixTransform($matrix, $x, $y) {
    $xyMat = array(array($x), array($y), array(1));
    $newXY = array(array());

    for ($i = 0; $i < 3; $i++) {
      for ($j = 0; $j < 1; $j++) {
        $sum = 0;

        for ($k = 0; $k < 3; $k++) {
          $sum += $matrix[$i][$k] * $xyMat[$k][$j];
        }

        $newXY[$i][$j] = $sum;
      }
    }

    /* Return the coordinates as a vector */
    return array($newXY[0][0], $newXY[1][0], $newXY[2][0]);
  }

  /*
   * Translate the X and Y coordinates. tX and tY specify the distance to
   * transform.
   */
  private function translateTransform($tX, $tY, $x, $y) {
    $matrix = array(array(1, 0, $tX), array(0, 1, $tY), array(0, 0, 1));
    return $this->matrixTransform($matrix, $x, $y);
  }

  /*
   * A2S_Scale transformations are implemented by applying the scale to the X and
   * Y coordinates. One unit in the new coordinate system equals $s[XY] units
   * in the old system. Thus, if you want to double the size of an object on
   * both axes, you sould call scaleTransform(0.5, 0.5, $x, $y)
   */
  private function scaleTransform($sX, $sY, $x, $y) {
    $matrix = array(array($sX, 0, 0), array(0, $sY, 0), array(0, 0, 1));
    return $this->matrixTransform($matrix, $x, $y);
  }

  /*
   * Rotate the coordinates around the center point cX and cY. If these
   * are not specified, the coordinate is rotated around 0,0. The angle
   * is specified in degrees.
   */
  private function rotateTransform($angle, $x, $y, $cX = 0, $cY = 0) {
    $angle = $angle * (pi() / 180);
    if ($cX != 0 || $cY != 0) {
      list ($x, $y) = $this->translateTransform($cX, $cY, $x, $y);
    }

    $matrix = array(array(cos($angle), -sin($angle), 0),
                    array(sin($angle), cos($angle), 0),
                    array(0, 0, 1));
    $ret = $this->matrixTransform($matrix, $x, $y);

    if ($cX != 0 || $cY != 0) {
      list ($x, $y) = $this->translateTransform(-$cX, -$cY, $ret[0], $ret[1]);
      $ret[0] = $x;
      $ret[1] = $y;
    }

    return $ret;
  }

  /*
   * Skews along the X axis at specified angle. The angle is specified in
   * degrees.
   */
  private function skewXTransform($angle, $x, $y) {
    $angle = $angle * (pi() / 180);
    $matrix = array(array(1, tan($angle), 0), array(0, 1, 0), array(0, 0, 1));
    return $this->matrixTransform($matrix, $x, $y);
  }

  /*
   * Skews along the Y axis at specified angle. The angle is specified in
   * degrees.
   */
  private function skewYTransform($angle, $x, $y) {
    $angle = $angle * (pi() / 180);
    $matrix = array(array(1, 0, 0), array(tan($angle), 1, 0), array(0, 0, 1));
    return $this->matrixTransform($matrix, $x, $y);
  }

  /*
   * Apply a transformation to a point $p.
   */
  private function applyTransformToPoint($txf, $p, $args) {
    switch ($txf) {
    case 'translate':
      return $this->translateTransform($args[0], $args[1], $p->x, $p->y);

    case 'scale':
      return $this->scaleTransform($args[0], $args[1], $p->x, $p->y);

    case 'rotate':
      if (count($args) > 1) {
        return  $this->rotateTransform($args[0], $p->x, $p->y, $args[1], $args[2]);
      } else {
        return  $this->rotateTransform($args[0], $p->x, $p->y);
      }

    case 'skewX':
      return $this->skewXTransform($args[0], $p->x, $p->y);

    case 'skewY':
      return $this->skewYTransform($args[0], $p->x, $p->y);
    }
  }

  /*
   * Apply the transformation function $txf to all coordinates on path $p
   * providing $args as arguments to the transformation function.
   */
  private function applyTransformToPath($txf, &$p, $args) {
    $pathCmds = count($p['path']);
    $curPoint = new A2S_Point(0, 0);
    $prevType = null;
    $curType = null;

    for ($i = 0; $i < $pathCmds; $i++) {
      $cmd = &$p['path'][$i];

      $prevType = $curType;
      $curType = $cmd[0];

      switch ($curType) {
      case 'z':
      case 'Z':
        /* Can't transform this */
        break;

      case 'm':
        if ($prevType != null) {
          $curPoint->x += $cmd[1];
          $curPoint->y += $cmd[2];

          list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
          $curPoint->x = $x;
          $curPoint->y = $y;

          $cmd[1] = $x;
          $cmd[2] = $y;
        } else {
          $curPoint->x = $cmd[1];
          $curPoint->y = $cmd[2];

          list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
          $curPoint->x = $x;
          $curPoint->y = $y;

          $cmd[1] = $x;
          $cmd[2] = $y;
          $curType = 'l';
        }

        break;

      case 'M':
        $curPoint->x = $cmd[1];
        $curPoint->y = $cmd[2];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $x;
        $cmd[2] = $y;

        if ($prevType == null) {
          $curType = 'L';
        }
        break;

      case 'l':
        $curPoint->x += $cmd[1];
        $curPoint->y += $cmd[2];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $x;
        $cmd[2] = $y;

        break;

      case 'L':
        $curPoint->x = $cmd[1];
        $curPoint->y = $cmd[2];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $x;
        $cmd[2] = $y;

        break;

      case 'v':
        $curPoint->y += $cmd[1];
        $curPoint->x += 0;

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $y;

        break;

      case 'V':
        $curPoint->y = $cmd[1];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $y;

        break;

      case 'h':
        $curPoint->x += $cmd[1];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $x;

        break;

      case 'H':
        $curPoint->x = $cmd[1];

        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;

        $cmd[1] = $x;

        break;

      case 'c':
        $tP = new A2S_Point(0, 0);
        $tP->x = $curPoint->x + $cmd[1]; $tP->y = $curPoint->y + $cmd[2];
        list ($x, $y) = $this->applyTransformToPoint($txf, $tP, $args);
        $cmd[1] = $x;
        $cmd[2] = $y;

        $tP->x = $curPoint->x + $cmd[3]; $tP->y = $curPoint->y + $cmd[4];
        list ($x, $y) = $this->applyTransformToPoint($txf, $tP, $args);
        $cmd[3] = $x;
        $cmd[4] = $y;

        $curPoint->x += $cmd[5];
        $curPoint->y += $cmd[6];
        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);

        $curPoint->x = $x;
        $curPoint->y = $y;
        $cmd[5] = $x;
        $cmd[6] = $y;

        break;
      case 'C':
        $curPoint->x = $cmd[1];
        $curPoint->y = $cmd[2];
        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $cmd[1] = $x;
        $cmd[2] = $y;

        $curPoint->x = $cmd[3];
        $curPoint->y = $cmd[4];
        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $cmd[3] = $x;
        $cmd[4] = $y;

        $curPoint->x = $cmd[5];
        $curPoint->y = $cmd[6];
        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);

        $curPoint->x = $x;
        $curPoint->y = $y;
        $cmd[5] = $x;
        $cmd[6] = $y;

        break;

      case 's':
      case 'S':

      case 'q':
      case 'Q':
        
      case 't':
      case 'T':

      case 'a':
        break;

      case 'A':
        /*
         * This radius is relative to the start and end points, so it makes
         * sense to scale, rotate, or skew it, but not translate it.
         */
        if ($txf != 'translate') {
          $curPoint->x = $cmd[1];
          $curPoint->y = $cmd[2];
          list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
          $cmd[1] = $x;
          $cmd[2] = $y;
        }

        $curPoint->x = $cmd[6];
        $curPoint->y = $cmd[7];
        list ($x, $y) = $this->applyTransformToPoint($txf, $curPoint, $args);
        $curPoint->x = $x;
        $curPoint->y = $y;
        $cmd[6] = $x;
        $cmd[7] = $y;

        break;
      }
    }
  }

  public function render() {
    $startPoint = array_shift($this->points);
    $endPoint = $this->points[count($this->points) - 1];

    $out = "<g id=\"group{$this->name}\">\n";

    /*
     * If someone has specified one of our special object types, we are going
     * to want to completely override any of the pathing that we would have
     * done otherwise, but we defer until here to do anything about it because
     * we need information about the object we're replacing.
     */
    if (isset($this->options['a2s:type']) &&
        isset(A2S_CustomObjects::$objects[$this->options['a2s:type']])) {
      $object = A2S_CustomObjects::$objects[$this->options['a2s:type']];

      /* Again, if no fill was specified, specify one. */
      if (!isset($this->options['fill'])) {
        $this->options['fill'] = '#fff';
      }

      /*
       * We don't care so much about the area, but we do care about the width
       * and height of the object. All of our "custom" objects are implemented
       * in 100x100 space, which makes the transformation marginally easier.
       */
      $minX = $startPoint->x; $maxX = $minX;
      $minY = $startPoint->y; $maxY = $minY;
      foreach ($this->points as $p) {
        if ($p->x < $minX) {
          $minX = $p->x;
        } elseif ($p->x > $maxX) {
          $maxX = $p->x;
        }
        if ($p->y < $minY) {
          $minY = $p->y;
        } elseif ($p->y > $maxY) {
          $maxY = $p->y;
        }
      }

      $objW = $maxX - $minX;
      $objH = $maxY - $minY;

      $i = 0;
      foreach ($object as $o) {
        $id = self::$id++;
        $out .= "\t<path id=\"path{$this->name}\" d=\"";

        $oW = $o['width'];
        $oH = $o['height'];

        $this->applyTransformToPath('scale', $o, array($objW/$oW, $objH/$oH));
        $this->applyTransformToPath('translate', $o, array($minX, $minY));

        foreach ($o['path'] as $cmd) {
          $out .= join(' ', $cmd) . ' ';
        }
        $out .= '" ';

        /* Don't add options to sub-paths */
        if ($i++ < 1) {
          foreach ($this->options as $opt => $val) {
            if (strpos($opt, 'a2s:', 0) === 0) {
              continue;
            }
            $out .= "$opt=\"$val\" ";
          }
        }

        $out .= " />\n";
      }

      if (count($this->text) > 0) {
        foreach ($this->text as $text) {
          $out .= "\t" . $text->render() . "\n";
        }
      }
      $out .= "</g>\n";

      /* Bazinga. */
      return $out;
    }

    /*
     * Nothing fancy here -- this is just rendering for our standard
     * polygons.
     *
     * Our start point is represented by a single moveto command (unless the
     * start point is curved) as the shape will be closed with the Z command
     * automatically if it is a closed shape. If we have a control point, we
     * have to go ahead and draw the curve.
     */
    if (($startPoint->flags & A2S_Point::CONTROL)) {
      $cX = $startPoint->x;
      $cY = $startPoint->y;
      $sX = $startPoint->x;
      $sY = $startPoint->y + 10;
      $eX = $startPoint->x + 10;
      $eY = $startPoint->y;

      $path = "M {$sX} {$sY} Q {$cX} {$cY} {$eX} {$eY} ";
    } else {
      $path = "M {$startPoint->x} {$startPoint->y} ";
    }

    $prevP = $startPoint;
    $bound = count($this->points);
    for ($i = 0; $i < $bound; $i++) {
      $p = $this->points[$i];

      /*
       * Handle quadratic Bezier curves. NOTE: This algorithm for drawing
       * the curves only works if the shapes are drawn in a clockwise
       * manner.
       */
      if (($p->flags & A2S_Point::CONTROL)) {
        /* Our control point is always the original corner */
        $cX = $p->x;
        $cY = $p->y;

        /* Need next point to determine which way to turn */
        if ($i == count($this->points) - 1) {
          $nP = $startPoint;
        } else {
          $nP = $this->points[$i + 1];
        }

        if ($prevP->x == $p->x) {
          /*
           * If we are on the same vertical axis, our starting X coordinate
           * is the same as the control point coordinate.
           */
          $sX = $p->x;
        
          /* Offset start point from control point in the proper direction */
          if ($prevP->y < $p->y) {
            $sY = $p->y - 10;
          } else {
            $sY = $p->y + 10;
          }

          $eY = $p->y;
          /* Offset end point from control point in the proper direction */
          if ($nP->x < $p->x) {
            $eX = $p->x - 10;
          } else {
            $eX = $p->x + 10;
          }
        } elseif ($prevP->y == $p->y) {
          /* Horizontal decisions mirror vertical's above */
          $sY = $p->y;
          if ($prevP->x < $p->x) {
            $sX = $p->x - 10;
          } else {
            $sX = $p->x + 10;
          }

          $eX = $p->x;
          if ($nP->y <= $p->y) {
            $eY = $p->y - 10;
          } else {
            $eY = $p->y + 10;
          }
        }

        $path .= "L {$sX} {$sY} Q {$cX} {$cY} {$eX} {$eY} ";
      } else {
        /* The excruciating difficulty of drawing a straight line */
        $path .= "L {$p->x} {$p->y} ";
      }

      $prevP = $p;
    }

    if ($this->isClosed()) {
      $path .= 'Z';
    } 

    $id = self::$id++;

    /* Add markers if necessary. */
    if ($startPoint->flags & A2S_Point::SMARKER) {
      $this->options["marker-start"] = "url(#Pointer)";
    } elseif ($startPoint->flags & A2S_Point::IMARKER) {
      $this->options["marker-start"] = "url(#iPointer)";
    }

    if ($endPoint->flags & A2S_Point::SMARKER) {
      $this->options["marker-end"] = "url(#Pointer)";
    } elseif ($endPoint->flags & A2S_Point::IMARKER) {
      $this->options["marker-end"] = "url(#iPointer)";
    }

    /*
     * SVG objects without a fill will be transparent, and this looks so
     * terrible with the drop-shadow effect. Any objects that aren't filled
     * automatically get a white fill.
     */
    if ($this->isClosed() && !isset($this->options['fill'])) {
      $this->options['fill'] = '#fff';
    }


    $out .= "\t<path id=\"path{$this->name}\" ";
    foreach ($this->options as $opt => $val) {
      if (strpos($opt, 'a2s:', 0) === 0) {
        continue;
      }
      $out .= "$opt=\"$val\" ";
    }
    $out .= "d=\"{$path}\" />\n";
    
    if (count($this->text) > 0) {
      foreach ($this->text as $text) {
        $text->setID($this->name);
        $out .= "\t" . $text->render() . "\n";
      }
    }

    $bound = count($this->ticks);
    for ($i = 0; $i < $bound; $i++) {
      $t = $this->ticks[$i];
      if ($t->flags & A2S_Point::DOT) {
        $out .= "<circle cx=\"{$t->x}\" cy=\"{$t->y}\" r=\"3\" fill=\"black\" />";
      } elseif ($t->flags & A2S_Point::TICK) {
        $x1 = $t->x - 4;
        $y1 = $t->y - 4;
        $x2 = $t->x + 4;
        $y2 = $t->y + 4;
        $out .= "<line x1=\"$x1\" y1=\"$y1\" x2=\"$x2\" y2=\"$y2\" stroke-width=\"1\" />";

        $x1 = $t->x + 4;
        $y1 = $t->y - 4;
        $x2 = $t->x - 4;
        $y2 = $t->y + 4;
        $out .= "<line x1=\"$x1\" y1=\"$y1\" x2=\"$x2\" y2=\"$y2\" stroke-width=\"1\" />";
      }
    }

    $out .= "</g>\n";
    return $out;
  }
}

/*
 * Nothing really special here. Container for representing text bits.
 */
class A2S_SVGText {
  private $options;
  private $string;
  private $point;
  private $name;

  private static $id = 0;

  public function __construct($x, $y) {
    $this->point = new A2S_Point($x, $y);
    $this->name = self::$id++;
    $this->options = array();
  }

  public function setOption($opt, $val) {
    $this->options[$opt] = $val;
  }

  public function setID($id) {
    $this->name = str_replace(' ', '_', str_replace('"', '_', $id));
  }

  public function getID() {
    return $this->name;
  }

  public function getPoint() {
    return $this->point;
  }

  public function setString($string) {
    $this->string = $string;
  }

  public function render() {
    $out = "<text x=\"{$this->point->x}\" y=\"{$this->point->y}\" id=\"text{$this->name}\" ";
    foreach ($this->options as $opt => $val) {
      if (strpos($opt, 'a2s:', 0) === 0) {
        continue;
      }
      $out .= "$opt=\"$val\" ";
    }
    $out .= ">";
    $out .= htmlentities($this->string);
    $out .= "</text>\n";
    return $out;
  }
}

/*
 * Main class for parsing ASCII and constructing the SVG output based on the
 * above classes.
 */
class A2S_ASCIIToSVG {
  private $rawData;
  private $grid;

  private $svgObjects;
  private $clearCorners;

  /* Directions for traversing lines in our grid */
  const DIR_UP    = 0x1;
  const DIR_DOWN  = 0x2;
  const DIR_LEFT  = 0x4;
  const DIR_RIGHT = 0x8;
  const DIR_NE    = 0x10;
  const DIR_SE    = 0x20;

  public function __construct($data) {
    /* For debugging purposes */
    $this->rawData = $data;

    A2S_CustomObjects::loadObjects();

    $this->clearCorners = array();

    /*
     * Parse out any command references. These need to be at the bottom of the
     * diagram due to the way they're removed. Format is:
     * [identifier] optional-colon optional-spaces ({json-blob})\n
     *
     * The JSON blob may not contain objects as values or the regex will break.
     */
    $this->commands = array();
    preg_match_all('/^\[([^\]]+)\]:?\s+({[^}]+?})/ims', $data, $matches);
    $bound = count($matches[1]);
    for ($i = 0; $i < $bound; $i++) {
      $this->commands[$matches[1][$i]] = json_decode($matches[2][$i], true);
    }

    $data = preg_replace('/^\[([^\]]+)\](:?)\s+.*/ims', '', $data);

    /*
     * Treat our ASCII field as a grid and store each character as a point in
     * that grid. The (0, 0) coordinate on this grid is top-left, just as it
     * is in images.
     */
    $this->grid = explode("\n", $data);

    foreach ($this->grid as $k => $line) {
      $this->grid[$k] = str_split($line);
    }

    $this->svgObjects = new A2S_SVGGroup();
  }

  /*
   * This is kind of a stupid and hacky way to do this, but this allows setting
   * the default scale of one grid space on the X and Y axes.
   */
  public function setDimensionScale($x, $y) {
    $o = A2S_Scale::getInstance();
    $o->setScale($x, $y);
  }

  public function dump() {
    var_export($this);
  }

  /* Render out what we've done!  */
  public function render() {
    $o = A2S_Scale::getInstance();

    /* Figure out how wide we need to make the canvas */
    $canvasWidth = 0;
    foreach($this->grid as $line) {
      if (count($line) > $canvasWidth) {
        $canvasWidth = count($line);
      }
    }

    /* Add a fudge factor for drop-shadow and gaussian blur */
    $canvasWidth = $canvasWidth * $o->xScale + 30;
    $canvasHeight = count($this->grid) * $o->yScale + 30;

    /*
     * Boilerplate header with definitions that we might be using for markers
     * and drop shadows.
     */
    $out = <<<SVG
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" 
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Created with A2S_ASCIIToSVG (http://9vx.org/~dho/a2s/) -->
<svg width="{$canvasWidth}px" height="{$canvasHeight}px" version="1.1"
  xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs>
    <filter id="dsFilter" width="150%" height="150%">
      <feOffset result="offOut" in="SourceGraphic" dx="3" dy="3"/>
      <feColorMatrix result="matrixOut" in="offOut" type="matrix" values="0.2 0 0 0 0 0 0.2 0 0 0 0 0 0.2 0 0 0 0 0 1 0"/>
      <feGaussianBlur result="blurOut" in="matrixOut" stdDeviation="3"/>
      <feBlend in="SourceGraphic" in2="blurOut" mode="normal"/>
    </filter>
    <marker id="iPointer"
      viewBox="0 0 10 10" refX="5" refY="5" 
      markerUnits="strokeWidth"
      markerWidth="8" markerHeight="7"
      orient="auto">
      <path d="M 10 0 L 10 10 L 0 5 z" />
    </marker>
    <marker id="Pointer"
      viewBox="0 0 10 10" refX="5" refY="5" 
      markerUnits="strokeWidth"
      markerWidth="8" markerHeight="7"
      orient="auto">
      <path d="M 0 0 L 10 5 L 0 10 z" />
    </marker>
  </defs>
SVG;

    /* Render the group, everything lives in there */
    $out .= $this->svgObjects->render();

    $out .= "</svg>\n";

    return $out;
  }

  /*
   * Parsing the grid is a multi-step process. We parse out boxes first, as
   * this makes it easier to then parse lines. By parse out, I do mean we
   * parse them and then remove them. This does mean that a complete line
   * will not travel along the edge of a box, but you probably won't notice
   * unless the box is curved anyway. While edges are removed, points are
   * not. This means that you can cleanly allow lines to intersect boxes
   * (as long as they do not bisect!
   *
   * After parsing boxes and lines, we remove the corners from the grid. At
   * this point, all we have left should be text, which we can pick up and
   * place.
   */
  public function parseGrid() {
    $this->parseBoxes();
    $this->parseLines();

    foreach ($this->clearCorners as $corner) {
      $this->grid[$corner[0]][$corner[1]] = ' ';
    }

    $this->parseText();

    $this->injectCommands();
  }

  /*
   * Ahh, good ol' box parsing. We do this by scanning each row for points and
   * attempting to close the shape. Since the approach is first horizontal,
   * then vertical, we complete the shape in a clockwise order (which is
   * important for the Bezier curve generation.
   */
  private function parseBoxes() {
    /* Set up our box group  */
    $this->svgObjects->pushGroup('boxes');
    $this->svgObjects->setOption('stroke', 'black');
    $this->svgObjects->setOption('stroke-width', '2');
    $this->svgObjects->setOption('fill', 'none');

    /* Scan the grid for corners */
    foreach ($this->grid as $row => $line) {
      foreach ($line as $col => $char) {
        if ($this->isCorner($char)) {
          $path = new A2S_SVGPath();

          if ($char == '.' || $char == "'") {
            $path->addPoint($col, $row, A2S_Point::CONTROL);
          } else {
            $path->addPoint($col, $row);
          }

          /*
           * The wall follower is a left-turning, marking follower. See that
           * function for more information on how it works.
           */
          $this->wallFollow($path, $row, $col+1, self::DIR_RIGHT);
        
          /* We only care about closed polygons */
          if ($path->isClosed()) {
            $path->orderPoints();

            $skip = false;
            /*
             * The walking code can find the same box from a different edge:
             *
             * +---+   +---+
             * |   |   |   |
             * |   +---+   |
             * +-----------+
             *
             * so ignore adding a box that we've already added.
             */
            foreach($this->svgObjects->getGroup('boxes') as $box) {
              $bP = $box->getPoints();
              $pP = $path->getPoints();
              $pPoints = count($pP);
              $shared = 0;

              /*
               * If the boxes don't have the same number of edges, they 
               * obviously cannot be the same box.
               */
              if (count($bP) != $pPoints) {
                continue;
              }

              /* Traverse the vertices of this new box... */
              for ($i = 0; $i < $pPoints; $i++) {
                /* ...and find them in this existing box. */
                for ($j = 0; $j < $pPoints; $j++) {
                  if ($pP[$i]->x == $bP[$j]->x && $pP[$i]->y == $bP[$j]->y) {
                    $shared++;
                  }
                }
              }

              /* If all the edges are in common, it's the same shape. */
              if ($shared == count($bP)) {
                $skip = true;
                break;
              }
            }

            if ($skip == false) {
              /* Search for any references for styling this polygon; add it */
              $path->setOption('filter', 'url(#dsFilter)');
              $name = $this->findCommands($path);

              if ($name != '') {
                $path->setID($name);
              }

              $this->svgObjects->addObject($path);
            }
          }
        }
      }
    }

    /*
     * Once we've found all the boxes, we want to remove them from the grid so
     * that they don't confuse the line parser. However, we don't remove any
     * corner characters because these might be shared by lines.
     */
    foreach ($this->svgObjects->getGroup('boxes') as $box) {
      $this->clearObject($box);
    }

    /* Anything after this is not a subgroup */
    $this->svgObjects->popGroup();
  }

  /*
   * Our line parser operates differently than the polygon parser. This is 
   * because lines are not intrinsically marked with starting points (markers
   * are optional) -- they just sort of begin. Additionally, so that markers
   * will work, we can't just construct a line from some random point: we need
   * to start at the correct edge.
   *
   * Thus, the line parser traverses vertically first, then horizontally. Once
   * a line is found, it is cleared immediately (but leaving any control points
   * in case there were any intersections.
   */
  private function parseLines() {
    /* Set standard line options */
    $this->svgObjects->pushGroup('lines');
    $this->svgObjects->setOption('stroke', 'black');
    $this->svgObjects->setOption('stroke-width', '2');
    $this->svgObjects->setOption('fill', 'none');

    /* The grid is not uniform, so we need to determine the longest row. */
    $maxCols = 0;
    $bound = count($this->grid);
    for ($r = 0; $r < $bound; $r++) {
      $maxCols = max($maxCols, count($this->grid[$r]));
    }

    for ($c = 0; $c < $maxCols; $c++) {
      for ($r = 0; $r < $bound; $r++) {
        /* This gets set if we find a line-start here. */
        $dir = false;

        $line = new A2S_SVGPath();

        /*
         * Since the column count isn't uniform, don't attempt to handle any
         * rows that don't extend out this far.
         */
        if (!isset($this->grid[$r][$c])) {
          continue;
        }

        $char = $this->getChar($r, $c);
        switch ($char) {
        /*
         * Do marker characters first. These are the easiest because they are
         * basically guaranteed to represent the start of the line.
         */
        case '<':
          $e = $this->getChar($r, $c + 1);
          if ($this->isEdge($e, self::DIR_RIGHT) || $this->isCorner($e)) {
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_RIGHT;
          } else {
            $se = $this->getChar($r + 1, $c + 1);
            $ne = $this->getChar($r - 1, $c + 1);
            if ($se == "\\") {
              $line->addMarker($c, $r, A2S_Point::IMARKER);
              $dir = self::DIR_SE;
            } elseif ($ne == '/') {
              $line->addMarker($c, $r, A2S_Point::IMARKER);
              $dir = self::DIR_NE;
            }
          }
          break;
        case '^':
          $s = $this->getChar($r + 1, $c);
          if ($this->isEdge($s, self::DIR_DOWN) || $this->isCorner($s)) { 
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_DOWN;
          } elseif ($this->getChar($r + 1, $c + 1) == "\\") {
            /* Don't need to check west for diagonals. */
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_SE;
          }
          break;
        case '>':
          $w = $this->getChar($r, $c - 1);
          if ($this->isEdge($w, self::DIR_LEFT) || $this->isCorner($w)) {
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_LEFT;
          }
          /* All diagonals come from west, so we don't need to check */
          break;
        case 'v':
          $n = $this->getChar($r - 1, $c);
          if ($this->isEdge($n, self::DIR_UP) || $this->isCorner($n)) {
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_UP;
          } elseif ($this->getChar($r - 1, $c + 1) == '/') {
            $line->addMarker($c, $r, A2S_Point::IMARKER);
            $dir = self::DIR_NE;
          }
          break;

        /*
         * Edges are handled specially. We have to look at the context of the
         * edge to determine whether it's the start of a line. A vertical edge
         * can appear as the start of a line in the following circumstances:
         *
         * +-------------      +--------------     +----    | (s)
         * |                   |                   |        |
         * |      | (s)        +-------+           |(s)     |
         * +------+                    | (s)
         *
         * From this we can extrapolate that we are a starting edge if our
         * southern neighbor is a vertical edge or corner, but we have no line
         * material to our north (and vice versa). This logic does allow for
         * the southern / northern neighbor to be part of a separate
         * horizontal line.
         */
        case ':':
          $line->setOption('stroke-dasharray', '5 5');
          /* FALLTHROUGH */
        case '|':
          $n = $this->getChar($r-1, $c);
          $s = $this->getChar($r+1, $c);
          if (($s == '|' || $s == ':' || $this->isCorner($s)) &&
              $n != '|' && $n != ':' && !$this->isCorner($n) &&
              $n != '^') {
            $dir = self::DIR_DOWN;
          } elseif (($n == '|' || $n == ':' || $this->isCorner($n)) &&
                    $s != '|' && $s != ':' && !$this->isCorner($s) &&
                    $s != 'v') {
            $dir = self::DIR_UP;
          }
          break;

        /*
         * Horizontal edges have the same properties for search as vertical
         * edges, except we need to look east / west. The diagrams for the
         * vertical case are still accurate to visualize this case; just
         * mentally turn them 90 degrees clockwise.
         */
        case '=':
          $line->setOption('stroke-dasharray', '5 5');
          /* FALLTHROUGH */
        case '-':
          $w = $this->getChar($r, $c-1);
          $e = $this->getChar($r, $c+1);
          if (($w == '-' || $w == '=' || $this->isCorner($w)) &&
              $e != '=' && $e != '-' && !$this->isCorner($e) &&
              $e != '>') {
            $dir = self::DIR_LEFT;
          } elseif (($e == '-' || $e == '=' || $this->isCorner($e)) &&
                    $w != '=' && $w != '-' && !$this->isCorner($w) &&
                    $w != '<') {
            $dir = self::DIR_RIGHT;
          }
          break;

        /*
         * We can only find diagonals going north or south and east. This is
         * simplified due to the fact that they have no corners. We are
         * guaranteed to run into their westernmost point or their relevant
         * marker.
         */
        case '/':
          $ne = $this->getChar($r-1, $c+1);
          if ($ne == '/' || $ne == '^' || $ne == '>') {
            $dir = self::DIR_NE;
          }
          break;

        case "\\":
          $se =  $this->getChar($r+1, $c+1);
          if ($se == "\\" || $se == "v" || $se == '>') {
            $dir = self::DIR_SE;
          }
          break;

        /*
         * The corner case must consider all four directions. Though a
         * reasonable person wouldn't use slant corners for this, they are
         * considered corners, so it kind of makes sense to handle them the
         * same way. For this case, envision the starting point being a corner
         * character in both the horizontal and vertical case. And then
         * mentally overlay them and consider that :).
         */
        case '+':
        case '#':
          $ne = $this->getChar($r-1, $c+1);
          $se =  $this->getChar($r+1, $c+1);
          if ($ne == '/' || $ne == '^' || $ne == '>') {
            $dir = self::DIR_NE;
          } elseif ($se == "\\" || $se == "v" || $se == '>') {
            $dir = self::DIR_SE;
          }
          /* FALLTHROUGH */

        case '.':
        case "'":
          $n = $this->getChar($r-1, $c);
          $w = $this->getChar($r, $c-1);
          $s = $this->getChar($r+1, $c);
          $e = $this->getChar($r, $c+1);
          if (($w == '=' || $w == '-') && $n != '|' && $n != ':' && $w != '-' &&
              $e != '=' && $e != '|' && $s != ':') {
            $dir = self::DIR_LEFT;
          } elseif (($e == '=' || $e == '-') && $n != '|' && $n != ':' && 
              $w != '-' && $w != '=' && $s != '|' && $s != ':') {
            $dir = self::DIR_RIGHT;
          } elseif (($s == '|' || $s == ':') && $n != '|' && $n != ':' &&
                    $w != '-' && $w != '=' && $e != '-' && $e != '=' &&
                    (($char != '.' && $char != "'") || 
                     ($char == '.' && $s != '.') || 
                     ($char == "'" && $s != "'"))) {
            $dir = self::DIR_DOWN;
          } elseif (($n == '|' || $n == ':') && $s != '|' && $s != ':' &&
                    $w != '-' && $w != '=' && $e != '-' && $e != '=' &&
                    (($char != '.' && $char != "'") || 
                     ($char == '.' && $s != '.') || 
                     ($char == "'" && $s != "'"))) {
            $dir = self::DIR_UP;
          }
          break;
        }

        /* It does actually save lines! */
        if ($dir !== false) {
          $rInc = 0; $cInc = 0;
          if (!$this->isMarker($char)) {
            $line->addPoint($c, $r);
          }

          /*
           * The walk routine may attempt to add the point again, so skip it.
           * If we don't, we can miss the line or end up with just a point.
           */
          if ($dir == self::DIR_UP) {
            $rInc = -1; $cInc = 0;
          } elseif ($dir == self::DIR_DOWN) {
            $rInc = 1; $cInc = 0;
          } elseif ($dir == self::DIR_RIGHT) {
            $rInc = 0; $cInc = 1;
          } elseif ($dir == self::DIR_LEFT) {
            $rInc = 0; $cInc = -1;
          } elseif ($dir == self::DIR_NE) {
            $rInc = -1; $cInc = 1;
          } elseif ($dir == self::DIR_SE) {
            $rInc = 1; $cInc = 1;
          }

          /*
           * Walk the points of this line. Note we don't use wallFollow; we are
           * operating under the assumption that lines do not meander. (And, in
           * any event, that algorithm is intended to find a closed object.)
           */
          $this->walk($line, $r+$rInc, $c+$cInc, $dir);

          /*
           * Remove it so that we don't confuse any other lines. This leaves
           * corners in tact, still.
           */
          $this->clearObject($line);
          $this->svgObjects->addObject($line);

          /* We may be able to find more lines starting from this same point */
          if ($this->isCorner($char)) {
            $r--;
          }
        }
      }
    }

    $this->svgObjects->popGroup();
  }

  /*
   * Look for text in a file. If the text appears in a box that has a dark
   * fill, we want to give it a light fill (and vice versa). This means we
   * have to figure out what box it lives in (if any) and do all sorts of
   * color calculation magic.
   */
  private function parseText() {
    $o = A2S_Scale::getInstance();

    /*
     * The style options deserve some comments. The monospace and font-size
     * choices are not accidental. This gives the best sort of estimation
     * for font size to scale that I could come up with empirically.
     *
     * N.B. This might change with different scales. I kind of feel like this
     * is a bug waiting to be filed, but whatever.
     */
    $fSize = 0.95*$o->yScale;
    $this->svgObjects->pushGroup('text');
    $this->svgObjects->setOption('fill', 'black');
    $this->svgObjects->setOption('style',
        "font-family:Consolas,Monaco,Anonymous Pro,Anonymous,Bitstream Sans Mono,monospace;font-size:{$fSize}px");

    /*
     * Text gets the same scanning treatment as boxes. We do left-to-right
     * scanning, which should probably be configurable in case someone wants
     * to use this with e.g. Arabic or some other right-to-left language.
     * Either way, this isn't UTF-8 safe (thanks, PHP!!!), so that'll require
     * thought regardless.
     */
    $boxes = $this->svgObjects->getGroup('boxes');
    $bound = count($boxes);

    foreach ($this->grid as $row => $line) {
      $cols = count($line);
      for ($i = 0; $i < $cols; $i++) {
        if ($this->getChar($row, $i) != ' ') {
          /* More magic numbers that probably need research. */
          $t = new A2S_SVGText($i - .6, $row + 0.3);

          /* Time to figure out which (if any) box we live inside */
          $tP = $t->getPoint();

          $maxPoint = new A2S_Point(-1, -1);
          $boxQueue = array();

          for ($j = 0; $j < $bound; $j++) {
            if ($boxes[$j]->hasPoint($tP->gridX, $tP->gridY)) {
              $boxPoints = $boxes[$j]->getPoints();
              $boxTL = $boxPoints[0];

              /*
               * This text is in this box, but it may still be in a more
               * specific nested box. Find the box with the highest top
               * left X,Y coordinate. Keep a queue of boxes in case the top
               * most box doesn't have a fill.
               */
              if ($boxTL->y > $maxPoint->y && $boxTL->x > $maxPoint->x) {
                $maxPoint->x = $boxTL->x;
                $maxPoint->y = $boxTL->y;
                $boxQueue[] = $boxes[$j];
              }
            }
          }

          if (count($boxQueue) > 0) {
            /*
             * Work backwards through the boxes to find the box with the most
             * specific fill.
             */
            for ($j = count($boxQueue) - 1; $j >= 0; $j--) {
              $fill = $boxQueue[$j]->getOption('fill');

              if ($fill == 'none' || $fill == null) {
                continue;
              }

              if (substr($fill, 0, 1) != '#') {
                if (!isset($GLOBALS['A2S_colors'][strtolower($fill)])) {
                  continue;
                } else {
                  $fill = $GLOBALS['A2S_colors'][strtolower($fill)];
                }
              } else {
                if (strlen($fill) != 4 && strlen($fill) != 7) {
                  continue;
                }
              }
                

              if ($fill) {
                /* Attempt to parse the fill color */
                if (strlen($fill) == 4) {
                  $cR = hexdec(str_repeat($fill[1], 2));
                  $cG = hexdec(str_repeat($fill[2], 2));
                  $cB = hexdec(str_repeat($fill[3], 2));
                } elseif (strlen($fill) == 7) {
                  $cR = hexdec(substr($fill, 1, 2));
                  $cG = hexdec(substr($fill, 3, 2));
                  $cB = hexdec(substr($fill, 5, 2));
                }

                /*
                 * This magic is gleaned from the working group paper on
                 * accessibility at http://www.w3.org/TR/AERT. The recommended
                 * contrast is a brightness difference of at least 125 and a
                 * color difference of at least 500. Since our default color
                 * is black, that makes the color difference easier.
                 */
                $bFill = (($cR * 299) + ($cG * 587) + ($cB * 114)) / 1000;
                $bDiff = $cR + $cG + $cB;
                $bText = 0;

                if ($bFill - $bText < 125 || $bDiff < 500) {
                  /* If black is too dark, white will work */
                  $t->setOption('fill', '#fff');
                } else {
                  $t->setOption('fill', '#000');
                }

                break;
              }
            }

            if ($j < 0) {
              $t->setOption('fill', '#000');
            }
          } else {
            /* This text isn't inside a box; make it black */
            $t->setOption('fill', '#000');
          }

          /* We found a stringy character, eat it and the rest. */
          $str = $this->getChar($row, $i++);
          while ($i < count($line) && $this->getChar($row, $i) != ' ') {
            $str .= $this->getChar($row, $i++);
            /* Eat up to 1 space */
            if ($this->getChar($row, $i) == ' ') {
              $str .= ' ';
              $i++;
            }
          }

          if ($str == '') {
            continue;
          }

          $t->setString($str);

          /*
           * If we were in a box, group with the box. Otherwise it gets its
           * own group.
           */
          if (count($boxQueue) > 0) {
            $t->setOption('stroke', 'none');
            $t->setOption('style',
              "font-family:Consolas,Monaco,Anonymous Pro,Anonymous,Bitstream Sans Mono,monospace;font-size:{$fSize}px");
            $boxQueue[count($boxQueue) - 1]->addText($t);
          } else {
            $this->svgObjects->addObject($t);
          }
        }
      }
    }
  }

  /*
   * Allow specifying references that target an object starting at grid point
   * (ROW,COL). This allows styling of lines, boxes, or any text object.
   */
  private function injectCommands() {
    $boxes = $this->svgObjects->getGroup('boxes');
    $lines = $this->svgObjects->getGroup('lines');
    $text = $this->svgObjects->getGroup('text');

    foreach ($boxes as $obj) {
      $objPoints = $obj->getPoints();
      $pointCmd = "{$objPoints[0]->gridY},{$objPoints[0]->gridX}";

      if (isset($this->commands[$pointCmd])) {
        $obj->setOptions($this->commands[$pointCmd]);
      }

      foreach ($obj->getText() as $text) {
        $textPoint = $text->getPoint();
        $pointCmd = "{$textPoint->gridY},{$textPoint->gridX}";

        if (isset($this->commands[$pointCmd])) {
          $text->setOptions($this->commands[$pointCmd]);
        }
      }
    }

    foreach ($lines as $obj) {
      $objPoints = $obj->getPoints();
      $pointCmd = "{$objPoints[0]->gridY},{$objPoints[0]->gridX}";

      if (isset($this->commands[$pointCmd])) {
        $obj->setOptions($this->commands[$pointCmd]);
      }
    }

    foreach ($text as $obj) {
      $objPoint = $obj->getPoint();
      $pointCmd = "{$objPoint->gridY},{$objPoint->gridX}";

      if (isset($this->commands[$pointCmd])) {
        $obj->setOptions($this->commands[$pointCmd]);
      }
    }
  }

  /*
   * A generic, recursive line walker. This walker makes the assumption that
   * lines want to go in the direction that they are already heading. I'm
   * sure that there are ways to formulate lines to screw this walker up,
   * but it does a good enough job right now.
   */
  private function walk($path, $row, $col, $dir, $d = 0) {
    $d++;
    $r = $row;
    $c = $col;

    if ($dir == self::DIR_RIGHT || $dir == self::DIR_LEFT) {
      $cInc = ($dir == self::DIR_RIGHT) ? 1 : -1;
      $rInc = 0;
    } elseif ($dir == self::DIR_DOWN || $dir == self::DIR_UP) {
      $cInc = 0;
      $rInc = ($dir == self::DIR_DOWN) ? 1 : -1;
    } elseif ($dir == self::DIR_SE || $dir == self::DIR_NE) {
      $cInc = 1;
      $rInc = ($dir == self::DIR_SE) ? 1 : -1;
    }

    /* Follow the edge for as long as we can */
    $cur = $this->getChar($r, $c);
    while ($this->isEdge($cur, $dir)) {
      if ($cur == ':' || $cur == '=') {
        $path->setOption('stroke-dasharray', '5 5');
      }

      if ($this->isTick($cur)) {
        $path->addTick($c, $r, ($cur == 'o') ? A2S_Point::DOT : A2S_Point::TICK);
        $path->addPoint($c, $r);
      }

      $c += $cInc;
      $r += $rInc;
      $cur = $this->getChar($r, $c);
    }

    if ($this->isCorner($cur)) {
      if ($cur == '.' || $cur == "'") {
        $path->addPoint($c, $r, A2S_Point::CONTROL);
      } else {
        $path->addPoint($c, $r);
      }

      if ($path->isClosed()) {
        $path->popPoint();
        return;
      }

      /*
       * Attempt first to continue in the current direction. If we can't,
       * try to go in any direction other than the one opposite of where
       * we just came from -- no backtracking.
       */
      $n = $this->getChar($r - 1, $c);
      $s = $this->getChar($r + 1, $c);
      $e = $this->getChar($r, $c + 1);
      $w = $this->getChar($r, $c - 1);
      $next = $this->getChar($r + $rInc, $c + $cInc);

      $se = $this->getChar($r + 1, $c + 1);
      $ne = $this->getChar($r - 1, $c + 1);

      if ($this->isCorner($next) || $this->isEdge($next, $dir)) {
        return $this->walk($path, $r + $rInc, $c + $cInc, $dir, $d);
      } elseif ($dir != self::DIR_DOWN &&
                ($this->isCorner($n) || $this->isEdge($n, self::DIR_UP))) {
        /* Can't turn up into bottom corner */
        if (($cur != '.' && $cur != "'") || ($cur == '.' && $n != '.') ||
              ($cur == "'" && $n != "'")) {
          return $this->walk($path, $r - 1, $c, self::DIR_UP, $d);
        }
      } elseif ($dir != self::DIR_UP && 
                ($this->isCorner($s) || $this->isEdge($s, self::DIR_DOWN))) {
        /* Can't turn down into top corner */
        if (($cur != '.' && $cur != "'") || ($cur == '.' && $s != '.') ||
              ($cur == "'" && $s != "'")) {
          return $this->walk($path, $r + 1, $c, self::DIR_DOWN, $d);
        }
      } elseif ($dir != self::DIR_LEFT &&
                ($this->isCorner($e) || $this->isEdge($e, self::DIR_RIGHT))) {
        return $this->walk($path, $r, $c + 1, self::DIR_RIGHT, $d);
      } elseif ($dir != self::DIR_RIGHT &&
                ($this->isCorner($w) || $this->isEdge($w, self::DIR_LEFT))) {
        return $this->walk($path, $r, $c - 1, self::DIR_LEFT, $d);
      } elseif ($dir == self::DIR_SE &&
                ($this->isCorner($ne) || $this->isEdge($ne, self::DIR_NE))) {
        return $this->walk($path, $r - 1, $c + 1, self::DIR_NE, $d);
      } elseif ($dir == self::DIR_NE &&
                ($this->isCorner($se) || $this->isEdge($se, self::DIR_SE))) {
        return $this->walk($path, $r + 1, $c + 1, self::DIR_SE, $d);
      }
    } elseif ($this->isMarker($cur)) {
      /* We found a marker! Add it. */
      $path->addMarker($c, $r, A2S_Point::SMARKER);
      return;
    } else {
      /*
       * Not a corner, not a marker, and we already ate edges. Whatever this
       * is, it is not part of the line.
       */
      $path->addPoint($c, $r);
      return;
    }
  }

  /*
   * This function attempts to follow a line and complete it into a closed
   * polygon. It assumes that we have been called from a top point, and in any
   * case that the polygon can be found by moving clockwise along its edges.
   * Any time this algorithm finds a corner, it attempts to turn right. If it
   * cannot turn right, it goes in any direction other than the one it came
   * from. If it cannot complete the polygon by continuing in any direction
   * from a point, that point is removed from the path, and we continue on
   * from the previous point (since this is a recursive function).
   *
   * Because the function assumes that it is starting from the top left,
   * if its first turn cannot be a right turn to moving down, the object
   * cannot be a valid polygon. It also maintains an internal list of points
   * it has already visited, and refuses to visit any point twice.
   */
  private function wallFollow($path, $r, $c, $dir, $bucket = array(), $d = 0) {
    $d++;

    if ($dir == self::DIR_RIGHT || $dir == self::DIR_LEFT) {
      $cInc = ($dir == self::DIR_RIGHT) ? 1 : -1;
      $rInc = 0;
    } elseif ($dir == self::DIR_DOWN || $dir == self::DIR_UP) {
      $cInc = 0;
      $rInc = ($dir == self::DIR_DOWN) ? 1 : -1;
    }

    /* Traverse the edge in whatever direction we are going. */
    $cur = $this->getChar($r, $c);
    while ($this->isBoxEdge($cur, $dir)) {
      $r += $rInc;
      $c += $cInc;
      $cur = $this->getChar($r, $c);
    }

    /* We 'key' our location by catting r and c together */
    $key = "{$r}{$c}";
    if (isset($bucket[$key])) {
      return;
    }

    /*
     * When we run into a corner, we have to make a somewhat complicated
     * decision about which direction to turn.
     */
    if ($this->isBoxCorner($cur)) {
      if (!isset($bucket[$key])) {
        $bucket[$key] = 0;
      }

      switch ($cur) {
      case '.':
      case "'":
        $pointExists = $path->addPoint($c, $r, A2S_Point::CONTROL);
        break;

      case '#':
        $pointExists = $path->addPoint($c, $r);
        break;
      }

      if ($path->isClosed() || $pointExists) {
        return;
      }

      /*
      * Special case: if we're looking for our first turn and we can't make it
      * due to incompatible corners, keep looking, but don't adjust our call
      * depth so that we can continue to make progress.
      */
      if ($d == 1 && $cur == '.' && $this->getChar($r + 1, $c) == '.') {
        return $this->wallFollow($path, $r, $c + 1, $dir, $bucket, 0);
      }

      /*
       * We need to make a decision here on where to turn. We may have multiple
       * directions we can choose, and all of them might generate a closed
       * object. Always try turning right first.
       */
      $newDir = false;
      $n = $this->getChar($r - 1, $c);
      $s = $this->getChar($r + 1, $c);
      $e = $this->getChar($r, $c + 1);
      $w = $this->getChar($r, $c - 1);

      if ($dir == self::DIR_RIGHT) {
        if (!($bucket[$key] & self::DIR_DOWN) &&
            ($this->isBoxEdge($s, self::DIR_DOWN) || $this->isBoxCorner($s))) {
          /* We can't turn into another top edge. */
          if (($cur != '.' && $cur != "'") || ($cur == '.' && $s != '.') ||
              ($cur == "'" && $s != "'")) {
            $newDir = self::DIR_DOWN;
          }
        } else {
          /* There is no right hand turn for us; this isn't a valid start */
          if ($d == 1) {
            return;
          }
        }
      } elseif ($dir == self::DIR_DOWN) {
        if (!($bucket[$key] & self::DIR_LEFT) &&
            ($this->isBoxEdge($w, self::DIR_LEFT) || $this->isBoxCorner($w))) {
          $newDir == self::DIR_LEFT;
        } 
      } elseif ($dir == self::DIR_LEFT) {
        if (!($bucket[$key] & self::DIR_UP) &&
            ($this->isBoxEdge($n, self::DIR_UP) || $this->isBoxCorner($n))) {
          /* We can't turn into another bottom edge. */
          if (($cur != '.' && $cur != "'") || ($cur == '.' && $n != '.') ||
              ($cur == "'" && $n != "'")) {
            $newDir = self::DIR_UP;
          }
        } 
      } elseif ($dir == self::DIR_UP) {
        if (!($bucket[$key] & self::DIR_RIGHT) &&
            ($this->isBoxEdge($e, self::DIR_RIGHT) || $this->isBoxCorner($e))) {
          $newDir = self::DIR_RIGHT;
        } 
      }

      if ($newDir != false) {
        if ($newDir == self::DIR_RIGHT || $newDir == self::DIR_LEFT) {
          $cMod = ($newDir == self::DIR_RIGHT) ? 1 : -1;
          $rMod = 0;
        } elseif ($newDir == self::DIR_DOWN || $newDir == self::DIR_UP) {
          $cMod = 0;
          $rMod = ($newDir == self::DIR_DOWN) ? 1 : -1;
        }

        $bucket[$key] |= $newDir;
        $this->wallFollow($path, $r+$rMod, $c+$cMod, $newDir, $bucket, $d);
        if ($path->isClosed()) {
          return;
        }
      }

      /*
       * Unfortunately, we couldn't complete the search by turning right,
       * so we need to pick a different direction. Note that this will also
       * eventually cause us to continue in the direction we were already
       * going. We make sure that we don't go in the direction opposite of
       * the one in which we're already headed, or an any direction we've
       * already travelled for this point (we may have hit it from an
       * earlier branch). We accept the first closing polygon as the
       * "correct" one for this object.
       */
      if ($dir != self::DIR_RIGHT && !($bucket[$key] & self::DIR_LEFT) &&
          ($this->isBoxEdge($w, self::DIR_LEFT) || $this->isBoxCorner($w))) {
        $bucket[$key] |= self::DIR_LEFT;
        $this->wallFollow($path, $r, $c - 1, self::DIR_LEFT, $bucket, $d);
        if ($path->isClosed()) {
          return;
        }
      } 
      if ($dir != self::DIR_LEFT && !($bucket[$key] & self::DIR_RIGHT) &&
          ($this->isBoxEdge($e, self::DIR_RIGHT) || $this->isBoxCorner($e))) {
        $bucket[$key] |= self::DIR_RIGHT;
        $this->wallFollow($path, $r, $c + 1, self::DIR_RIGHT, $bucket, $d);
        if ($path->isClosed()) {
          return;
        }
      } 
      if ($dir != self::DIR_DOWN && !($bucket[$key] & self::DIR_UP) &&
          ($this->isBoxEdge($n, self::DIR_UP) || $this->isBoxCorner($n))) {
          if (($cur != '.' && $cur != "'") || ($cur == '.' && $n != '.') ||
              ($cur == "'" && $n != "'")) {
          /* We can't turn into another bottom edge. */
          $bucket[$key] |= self::DIR_UP;
          $this->wallFollow($path, $r - 1, $c, self::DIR_UP, $bucket, $d);
          if ($path->isClosed()) {
            return;
          }
        }
      } 
      if ($dir != self::DIR_UP && !($bucket[$key] & self::DIR_DOWN) &&
          ($this->isBoxEdge($s, self::DIR_DOWN) || $this->isBoxCorner($s))) {
          if (($cur != '.' && $cur != "'") || ($cur == '.' && $s != '.') ||
              ($cur == "'" && $s != "'")) {
          /* We can't turn into another top edge. */
          $bucket[$key] |= self::DIR_DOWN;
          $this->wallFollow($path, $r + 1, $c, self::DIR_DOWN, $bucket, $d);
          if ($path->isClosed()) {
            return;
          }
        }
      }

      /*
       * If we get here, the path doesn't close in any direction from this
       * point (it's probably a line extension). Get rid of the point from our
       * path and go back to the last one.
       */
      $path->popPoint();
      return;
    } elseif ($this->isMarker($this->getChar($r, $c))) {
      /* Marker is part of a line, not a wall to close. */
      return;
    } else {
      /* We landed on some whitespace or something; this isn't a closed path */
      return;
    }
  }

  /*
   * Clears an object from the grid, erasing all edge and marker points. This
   * function retains corners in "clearCorners" to be cleaned up before we do
   * text parsing.
   */
  private function clearObject($obj) {
    $points = $obj->getPoints();
    $closed = $obj->isClosed();

    $bound = count($points);
    for ($i = 0; $i < $bound; $i++) {
      $p = $points[$i];

      if ($i == count($points) - 1) {
        /* This keeps us from handling end of line to start of line */
        if ($closed) {
          $nP = $points[0];
        } else {
          $nP = null;
        }
      } else {
        $nP = $points[$i+1];
      }

      /* If we're on the same vertical axis as our next point... */
      if ($nP != null && $p->gridX == $nP->gridX) {
        /* ...traverse the vertical line from the minimum to maximum points */
        $maxY = max($p->gridY, $nP->gridY);
        for ($j = min($p->gridY, $nP->gridY); $j <= $maxY; $j++) {
          $char = $this->getChar($j, $p->gridX);

          if (!$this->isTick($char) && $this->isEdge($char) || $this->isMarker($char)) {
            $this->grid[$j][$p->gridX] = ' ';
          } elseif ($this->isCorner($char)) {
            $this->clearCorners[] = array($j, $p->gridX);
          } elseif ($this->isTick($char)) {
            $this->grid[$j][$p->gridX] = '+';
          }
        }
      } elseif ($nP != null && $p->gridY == $nP->gridY) {
        /* Same horizontal plane; traverse from min to max point */
        $maxX = max($p->gridX, $nP->gridX);
        for ($j = min($p->gridX, $nP->gridX); $j <= $maxX; $j++) {
          $char = $this->getChar($p->gridY, $j);

          if (!$this->isTick($char) && $this->isEdge($char) || $this->isMarker($char)) {
            $this->grid[$p->gridY][$j] = ' ';
          } elseif ($this->isCorner($char)) {
            $this->clearCorners[] = array($p->gridY, $j);
          } elseif ($this->isTick($char)) {
            $this->grid[$p->gridY][$j] = '+';
          }
        }
      } elseif ($nP != null && $closed == false && $p->gridX != $nP->gridX &&
                $p->gridY != $nP->gridY) {
        /*
         * This is a diagonal line starting from the westernmost point. It
         * must contain max(p->gridY, nP->gridY) - min(p->gridY, nP->gridY)
         * segments, and we can tell whether to go north or south depending
         * on which side of zero p->gridY - nP->gridY lies. There are no
         * corners in diagonals, so we don't have to keep those around.
         */
        $c = $p->gridX;
        $r = $p->gridY;
        $rInc = ($p->gridY > $nP->gridY) ? -1 : 1;
        $bound = max($p->gridY, $nP->gridY) - min($p->gridY, $nP->gridY);

        /*
         * This looks like an off-by-one, but it is not. This clears the
         * corner, if one exists.
         */
        for ($j = 0; $j <= $bound; $j++) {
          $char = $this->getChar($r, $c);
          if ($char == '/' || $char == "\\" || $this->isMarker($char)) {
            $this->grid[$r][$c++] = ' ';
          } elseif ($this->isCorner($char)) {
            $this->clearCorners[] = array($r, $c++);
          } elseif ($this->isTick($char)) {
            $this->grid[$r][$c] = '+';
          }
          $r += $rInc;
        }

        $this->grid[$p->gridY][$p->gridX] = ' ';
        break;
      }
    }
  }

  /*
   * Find style information for this polygon. This information is required to
   * exist on the first line after the top, touching the left wall. It's kind
   * of a pain requirement, but there's not a much better way to do it:
   * ditaa's handling requires too much text flung everywhere and this way
   * gives you a good method for specifying *tons* of information about the
   * object.
   */
  private function findCommands($box) {
    $points = $box->getPoints();
    $sX = $points[0]->gridX + 1;
    $sY = $points[0]->gridY + 1;
    $ref = '';
    if ($this->getChar($sY, $sX++) == '[') {
      $char = $this->getChar($sY, $sX++);
      while ($char != ']') {
        $ref .= $char;
        $char = $this->getChar($sY, $sX++);
      }

      if ($char == ']') {
        $sX = $points[0]->gridX + 1;
        $sY = $points[0]->gridY + 1;

        if (!isset($this->commands[$ref]['a2s:delref']) &&
            !isset($this->commands[$ref]['a2s:label'])) {
          $this->grid[$sY][$sX] = ' ';
          $this->grid[$sY][$sX + strlen($ref) + 1] = ' ';
        } else {
          if (isset($this->commands[$ref]['a2s:label'])) {
            $label = $this->commands[$ref]['a2s:label'];
          } else {
            $label = null;
          }

          $len = strlen($ref) + 2;
          for ($i = 0; $i < $len; $i++) {
            if (strlen($label) > $i) {
              $this->grid[$sY][$sX + $i] = substr($label, $i, 1);
            } else {
              $this->grid[$sY][$sX + $i] = ' ';
            }
          }
        }

        if (isset($this->commands[$ref])) {
          $box->setOptions($this->commands[$ref]);
        }
      }
    }

    return $ref;
  }
  
  /*
   * Extremely useful debugging information to figure out what has been
   * parsed, especially when used in conjunction with clearObject.
   */
  private function dumpGrid() {
    foreach($this->grid as $lines) {
      echo implode('', $lines) . "\n";
    }
  }

  private function getChar($row, $col) {
    if (isset($this->grid[$row][$col])) {
      return $this->grid[$row][$col];
    }

    return null;
  }

  private function isBoxEdge($char, $dir = null) {
    if ($dir == null) {
      return $char == '-' || $char == '|' || char == ':' || $char == '=' || $char == '*' || $char == '+';
    } elseif ($dir == self::DIR_UP || $dir == self::DIR_DOWN) {
      return $char == '|' || $char == ':' || $char == '*' || $char == '+';
    } elseif ($dir == self::DIR_LEFT || $dir == self::DIR_RIGHT) {
      return $char == '-' || $char == '=' || $char == '*' || $char == '+';
    }
  }

  private function isEdge($char, $dir = null) {
    if ($char == 'o' || $char == 'x') {
      return true;
    }

    if ($dir == null) {
      return $char == '-' || $char == '|' || $char == ':' || $char == '=' || $char == '*' || $char == '/' || $char == "\\";
    } elseif ($dir == self::DIR_UP || $dir == self::DIR_DOWN) {
      return $char == '|' || $char == ':' || $char == '*';
    } elseif ($dir == self::DIR_LEFT || $dir == self::DIR_RIGHT) {
      return $char == '-' || $char == '=' || $char == '*';
    } elseif ($dir == self::DIR_NE) {
      return $char == '/';
    } elseif ($dir == self::DIR_SE) {
      return $char == "\\";
    }
  }

  private function isBoxCorner($char) {
    return $char == '.' || $char == "'" || $char == '#';
  }

  private function isCorner($char) {
    return $char == '.' || $char == "'" || $char == '#' || $char == '+';
  }

  private function isMarker($char) {
    return $char == 'v' || $char == '^' || $char == '<' || $char == '>';
  }

  private function isTick($char) {
    return $char == 'o' || $char == 'x';
  }
}

/* vim:ts=2:sw=2:et:
 *  * */
