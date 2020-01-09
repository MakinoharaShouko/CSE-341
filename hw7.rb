# CSE341, Programming Languages, Homework 7, hw7.rb (see also SML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
      (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
      real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
      if real_close(x1,x2)
        VerticalLine.new x1
      else
        m = (y1 - y2).to_f / (x1 - x2)
        b = y2 - m * x2
        Line.new(m,b)
      end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

# nothing on the 2D plane
class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end

# a point on the 2D plane represented in Cartesian coordinate system
class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  
  # evaluates the point to itself
  def eval_prog env
    self
  end

  # preprocesses to itself
  def preprocess_prog
    self
  end

  # shifts the point by adding the shift values to the current coordinate
  def shift(dx, dy)
    Point.new(@x + dx, @y + dy)
  end

  # lets the parameter intersects with the point
  def intersect other
    other.intersectPoint self
  end

  # intersects with another point
  def intersectPoint p
    # same points
    if real_close_point(@x, @y, p.x, p.y)
      self
    # no intersection
    else
      NoPoints.new
    end
  end

  # intersects with a line
  def intersectLine l
    # the point is on the line
    if real_close(@y, l.m * @x + l.b)
      self
    # no intersection
    else
      NoPoints.new
    end
  end

  # intersects with a vertical line
  def intersectVerticalLine vl
    # the point is on the line
    if real_close(@x, vl.x)
      self
    # no intersection
    else
      NoPoint.new
    end
  end

  # checks the intersection with a line segment when
  # the intersection with the line on which the segment lies is a point
  def intersectWithSegmentAsLineResult seg
    def inbetween(v, end1, end2)  # checks if a is within the bounds of two values
      (end1 - GeometryExpression::Epsilon <= v && v <= end2 + GeometryExpression::Epsilon) || (end2 - GeometryExpression::Epsilon <= v && v <= end1 + GeometryExpression::Epsilon)
    end
    # checks if the point is within the line segment
    if inbetween(@x, seg.x1, seg.x2) && inbetween(@y, seg.y1, seg.y2)
      self  # intersect at the point
    else
      NoPoints.new  # out of segment; no intersection
    end
  end
end

# a straight line in 2D plane represented with its slope and zero y-offset
class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end

  # evaluates to itself
  def eval_prog env
    self
  end

  # preprocesses to itself
  def preprocess_prog
    self
  end

  # shifts the line
  # the slope is unchanged while the offset is moved
  def shift(dx, dy)
    Line.new(@m, @b + dy - dx * @m)
  end

  # lets the parameter intersects with the line
  def intersect other
    other.intersectLine self
  end

  # intersects with a point
  # (lets the point intersect with the line)
  def intersectPoint p
    p.intersectLine self
  end

  # intersects with another line
  def intersectLine l
    if real_close(@m, l.m)
      if real_close(@b, l.b)
        self  # same line
      else NoPoints.new  # parallel line
      end
    else  # two lines intersect at some point
      x = (@b - l.b) / (l.m - @m)
      y = @m * x + @b
      Point.new(x, y)
    end
  end

  # intersects with a vertical line
  # returns the point at which the two lines intersect
  def intersectVerticalLine vl
    Point.new(vl.x, @m * vl.x + @b)
  end

  # the intersection of a geometry value with the line
  # on which the line segment lies is a line
  # the segment is part of the line
  # returns the line segment
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

# a vertical line reprsented by the x-value the line is on
class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x

  # initialize the vertical line by setting its x value
  def initialize x
    @x = x
  end

  # a vetical line evaluates to itself
  def eval_prog env
    self
  end

  # preprocesses to itself
  def preprocess_prog
    self
  end

  # shifts the vertical line by adding the change in x value to the current x
  # leaving the y value alone
  def shift(dx, dy)
    VerticalLine.new(@x + dx)
  end

  # intersects with another geometry value
  # asks the other value to call intersectVerticalLine on the vertical line
  def intersect other
    other.intersectVerticalLine self
  end
  
  # intersects with a point
  # calls the point's intersectVerticalLine on this vertical line
  def intersectPoint p
    p.intersectVerticalLine self
  end

  # intersects with a line
  # calls the line's intersectVerticalLine on this vertical line
  def intersectLine l
    l.intersectVerticalLine self
  end

  # intersects with another vertical line
  # if their x values are close, they are the same vertical line
  # otherwise there is no intersection
  def intersectVerticalLine vl
    if real_close(@x, vl.x)
      self
    else
      NoPoints.new
    end
  end

  # intersection with the line on which
  # the line segments lies is a vertical line
  # the line segment is part of the vertical line
  # returns the line segment
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

# a line segment on 2D plane represented by
# the start coordinate and the end coordinate
class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  # remember (x1,y1) is to the /right/ of (x2,y2) or /above/ if x1 is real
  # close to x2
  attr_reader :x1, :y1, :x2, :y2

  # initializes the line segment by setting the
  # coordinates of its start and end
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  # a line segment evaluates to itself 
  def eval_prog env
    self
  end

  # preprocesses the line segment
  # if the start and the end are really close
  # processes the segment to a point and returns it
  # else makes sure that the first end point has higher x
  # if x at both ends are the same (real close)
  # makes sure the first end point has higher y
  # otherwise leaves the line segment in the original way
  def preprocess_prog
    if real_close_point(@x1, @y1, @x2, @y2)
      Point.new(@x1, @y1)
    elsif (@x1 < @x2) || (real_close(@x1, @x2) && (@y1 < @y2))
      LineSegment.new(@x2, @y2, @x1, @y1)
    else
      self
    end
  end

  # shifts the line segment by adding the x difference to
  # x of coordinates and adding y difference to y of coordinates
  def shift(dx, dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end

  # intersects with another geometry value
  # asks the other value to calls its intersectLineSegment on this line segment
  def intersect other
    other.intersectLineSegment self
  end

  # intersects with a point
  # lets the point calls intersectLineSegment method
  def intersectPoint p
    p.intersectLineSegment self
  end

  # intersects with a line
  # lets the line calls intersectLineSegment method
  def intersectLine l
    l.intersectLineSegment self
  end

  # intersects with a vertical line
  # lets the vertical line calls intersectLineSegment method
  def intersectVerticalLine vl
    vl.intersectLineSegment self
  end

  # the result of intersecting with a line on which the input line segment lies is
  # another line segment
  # check to see if the original segment has any overlapping with the result segment
  def intersectWithSegmentAsLineResult seg
    s1 = seg
    s2 = self
    if real_close(@x1, @x2)
      if (@y2 < seg.y2)
        s1 = self
        s2 = seg
      end
      if real_close(s1.y1, s2.y2)
        Point.new(s1.x1, s1.y1)
      elsif s1.y1 < s2.y2
        NoPoints.new
      elsif s1.y1 > s2.y1
        s2
      else
        LineSegment.new(s1.x1, s1.y1, s2.x2, s2.y2)
      end
    else
      if (@x2 < seg.x2)
        s1 = self
        s2 = seg
      end
      if real_close(s1.x1, s2.x2)
        Point.new(s1.x1, s2.y1)
      elsif (s1.x1 < s2.x2)
        NoPoints.new
      elsif (s1.x1 > s2.x1)
        s2
      else
        LineSegment.new(s1.x1, s1.y1, s2.x2, s2.y2)
      end
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # initializes the intersect expression by setting the two internal expressions
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  # evaluates the intersect expression by intersecting
  # the result of evaluating two internal expressions
  def eval_prog env
    (@e1.eval_prog env).intersect(@e2.eval_prog env)
  end

  # preprocesses the two internal expressions
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let\

  # creates the local binding by setting the variable to bind to,
  # the expression to bind and the expression in which the local binding
  # is effective
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  # evaluates the local binding by adding the binding to the environment
  # and then evaluating
  def eval_prog env
    @e2.eval_prog env.unshift([@s, @e1])
  end

  # preproceses the local binding by preprocesses its two internal expressions
  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # create a new variable by setting its name
  def initialize s
    @s = s
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  # a variable preprocesses to itself
  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # initialize the shift expression by setting the x y values to shift with
  # and the expression to shift
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  # evaluates the expression and shifts it
  def eval_prog env
    (@e.eval_prog env).shift(@dx, @dy)
  end

  # preprocesses the internal expression
  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end
