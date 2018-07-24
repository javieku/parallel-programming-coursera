
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    if(radius == 0) {
      return src(x,y)
    }

    val initial_x = if (x - radius < 0) 0 else x - radius
    val initial_y = if (y - radius < 0) 0 else y - radius
    val final_x = if(x + radius >= src.width) src.width else x + radius + 1
    val final_y = if(y + radius >= src.height) src.height else y + radius + 1

    var current_y  = initial_y

    var total_red_sum = 0
    var total_green_sum = 0
    var total_blue_sum = 0
    var total_alpha_sum = 0
    var counter = 0

    while(current_y < final_y) {
      var current_x = initial_x
      while(current_x < final_x) {
        total_red_sum = total_red_sum + red(src(current_x,current_y))
        total_green_sum = total_green_sum + green(src(current_x,current_y))
        total_blue_sum = total_blue_sum + blue(src(current_x,current_y))
        total_alpha_sum = total_alpha_sum + alpha(src(current_x,current_y))
        counter = counter + 1
        current_x = current_x + 1
      }
      current_y = current_y + 1
    }

    rgba((total_red_sum/counter),(total_green_sum/counter),
         (total_blue_sum/counter),(total_alpha_sum/counter))
  }

}
