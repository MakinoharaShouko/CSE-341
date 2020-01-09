import java.util.*;

class NoElementException extends Exception {
  public NoElementException(String message){
      super(message);
  }
}

abstract class GeometryExpression {
   abstract GeometryValue eval_prog(Map<String, GeometryExpression> env);

   abstract GeometryExpression preprocess_prog();
}

abstract class GeometryValue extends GeometryExpression {
   final double EPSILON = 0.00001;

   // takes two doubles and returns if the two values are close to each other
   protected boolean real_close(double r1, double r2) {
      return Math.abs(r1 - r2) < EPSILON;
   }

   // takes two coordinates and returns if the two points are close to each other
   protected boolean real_close_point(double x1, double y1, double x2, double y2) {
      return real_close(x1, x2) && real_close(y1, y2);
   }

   protected GeometryValue two_points_to_line(double x1, double y1, double x2, double y2) {
      if (real_close(x1, x2)) {
         return new VerticalLine(x1);
      } else {
         double m = (y1 - y2) / (x1 - x2);
         double b = y2 - m * x2;
         return new Line(m, b);
      }
   }

   abstract GeometryValue eval_prog(Map<String, GeometryExpression> env);

   abstract GeometryValue preprocess_prog();

   abstract GeometryValue shift (double dx, double dy);

   public GeometryValue intersectNoPoints(NoPoints np) {
      return np;
   }

   public GeometryValue intersectLineSegment(LineSegment seg) {
      GeometryValue line_result = this.intersect(two_points_to_line(seg.x1(), seg.y1(), seg.x2(), seg.y2()));
      return line_result.intersectWithSegmentAsLineResult(seg);
   }

   abstract GeometryValue intersect(GeometryValue gv);

   abstract GeometryValue intersectPoint(Point p);

   abstract GeometryValue intersectLine(Line l);

   abstract GeometryValue intersectVerticalLine(VerticalLine vl);

   abstract GeometryValue intersectWithSegmentAsLineResult(LineSegment seg);
}

class NoPoints extends GeometryValue {
   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return this;
   }

   public GeometryValue preprocess_prog() {
      return this;
   }

   public GeometryValue shift(double dx, double dy) {
      return this;
   }

   public GeometryValue intersect(GeometryValue other) {
      return other.intersectNoPoints(this);
   }

   public GeometryValue intersectPoint(Point p) {
      return this;
   }

   public GeometryValue intersectLine(Line l) {
      return this;
   }

   public GeometryValue intersectVerticalLine(VerticalLine vl) {
      return this;
   }

   public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
      return this;
   }
}

class Point extends GeometryValue {
   private double x, y;

   Point(double x, double y) {
      this.x = x;
      this.y = y;
   }

   public double x() {
      return x;
   }

   public double y() {
      return y;
   }

   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return this;
   }

   public GeometryValue preprocess_prog() {
      return this;
   }

   public GeometryValue shift(double dx, double dy) {
      return new Point(x + dx, y + dy);
   }

   public GeometryValue intersect(GeometryValue other) {
      return other.intersectPoint(this);
   }

   public GeometryValue intersectPoint(Point p) {
      if (real_close_point(x, y, p.x, p.y)) {
         return this;
      }
      return new NoPoints();
   }

   public GeometryValue intersectLine(Line l) {
      if (real_close(y, l.m() * x + l.b())) {
         return this;
      }
      return new NoPoints();
   }

   public GeometryValue intersectVerticalLine(VerticalLine vl) {
      if (real_close(x, vl.x())) {
         return this;
      }
      return new NoPoints();
   }

   public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
      if (inbetween(x, seg.x1(), seg.x2()) && (inbetween(y, seg.y1(), seg.y2()))) {
         return this;
      }
      return new NoPoints();
   }

   private boolean inbetween(double v, double end1, double end2) {
      return (end1 - EPSILON <= v && v <= end2 + EPSILON) || (end2 - EPSILON <= v && v <= end1 + EPSILON);
   }
}

class Line extends GeometryValue {
   private double m, b;

   Line(double m, double b) {
      this.m = m;
      this.b = b;
   }

   public double m() {
      return m;
   }

   public double b() {
      return b;
   }

   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return this;
   }

   public GeometryValue preprocess_prog() {
      return this;
   }

   public GeometryValue shift(double dx, double dy) {
      return new Line(m, b + dy - dx * m);
   }

   public GeometryValue intersect(GeometryValue other) {
      return other.intersectLine(this);
   }

   public GeometryValue intersectPoint(Point p) {
      return p.intersectLine(this);
   }

   public GeometryValue intersectLine(Line l) {
      if (real_close(m, l.m)) {
         if (real_close(b, l.b)) {
            return this;
         }
         return new NoPoints();
      } else {
         double x = (b - l.b) / (l.m - m);
         double y = m * x + b;
         return new Point(x, y);
      }
   }

   public GeometryValue intersectVerticalLine(VerticalLine vl) {
      return new Point(vl.x(), m * vl.x() + b);
   }

   public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
      return seg;
   }
}

class VerticalLine extends GeometryValue {
   private double x;

   VerticalLine(double x) {
      this.x = x;
   }

   public double x() {
      return x;
   }

   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return this;
   }

   public GeometryValue preprocess_prog() {
      return this;
   }

   public GeometryValue shift (double dx, double dy) {
      return new VerticalLine (x + dx);
   }

   public GeometryValue intersect(GeometryValue other) {
      return other.intersectVerticalLine(this);
   }

   public GeometryValue intersectPoint(Point p) {
      return p.intersectVerticalLine(this);
   }

   public GeometryValue intersectLine(Line l) {
      return l.intersectVerticalLine(this);
   }

   public GeometryValue intersectVerticalLine(VerticalLine vl) {
      if (real_close(vl.x, x)) {
         return this;
      }
      return new NoPoints();
   }

   public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
      return seg;
   }
}

class LineSegment extends GeometryValue {
   private double x1, x2, y1, y2;

   LineSegment(double x1, double x2, double y1, double y2) {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
   }

   public double x1() {
      return x1;
   }

   public double x2() {
      return x2;
   }

   public double y1() {
      return y1;
   }

   public double y2() {
      return y2;
   }

   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return this;
   }

   public GeometryValue preprocess_prog() {
      if (real_close_point(x1, y1, x2, y2)) {
         return new Point(x1, y1);
      } else if ((x1 < x2) || (real_close(x1, x2) && (y1 < y2))) {
         return new LineSegment(x2, y2, x1, y1);
      }
      return this;
   }

   public GeometryValue shift(double dx, double dy) {
      return new LineSegment(x1 + dx, y1 + dy, x2 + dx, y2 + dy);
   }

   public GeometryValue intersect(GeometryValue other) {
      return other.intersectLineSegment(this);
   }

   public GeometryValue intersectPoint(Point p) {
      return p.intersectLineSegment(this);
   }

   public GeometryValue intersectLine(Line l) {
      return l.intersectLineSegment(this);
   }

   public GeometryValue intersectVerticalLine(VerticalLine vl) {
      return vl.intersectLineSegment(this);
   }

   public GeometryValue intersectWithSegmentAsLineResult(LineSegment seg) {
      LineSegment s1 = seg;
      LineSegment s2 = this;
      if (real_close(x1, x2)) {
         if (y2 < seg.y2) {
            s1 = this;
            s2 = seg;
         }
         if (real_close(s1.y1, s2.y2)) {
            return new Point(s1.x1, s1.y1);
         } else if (s1.y1 < s2.y2) {
            return new NoPoints();
         } else if (s1.y1 > s2.y1) {
            return s2;
         }
         return new LineSegment(s1.x1, s1.y1, s2.x2, s2.y2);
      } else {
         if (x2 < seg.x2) {
            s1 = this;
            s2 = seg;
         }
         if (real_close(s1.x1, s2.x2)) {
            return new Point(s1.x1, s2.y1);
         } else if (s1.x1 < s2.x2) {
            return new NoPoints();
         } else if (s1.x1 > s2.x1) {
            return s2;
         }
         return new LineSegment (s1.x1, s1.y1, s2.x2, s2.y2);
      }
   }
}

class Intersect extends GeometryExpression {
   private GeometryExpression e1, e2;

   Intersect(GeometryExpression e1, GeometryExpression e2) {
      this.e1 = e1;
      this.e2 = e2;
   }

   public GeometryValue eval_prog(Map<String, GeometryExpression> env) {
      return (e1.eval_prog(env)).intersect(e2.eval_prog(env));
   }

   public GeometryExpression preprocess_prog() {
      return new Intersect(e1.preprocess_prog(), e2.preprocess_prog());
   }
}

class Let extends GeometryExpression {
   private String s;
   private GeometryExpression e1;
   private GeometryExpression e2;

   Let (String s, GeometryExpression e1, GeometryExpression e2) {
      this.s = s;
      this.e1 = e1;
      this.e2 = e2;
   }

   public GeometryValue eval_prog (Map<String, GeometryExpression> env) {
      env.put(s, e1);
      return e2.eval_prog(env);
   }

   public GeometryExpression preprocess_prog () {
      return new Let(s, e1.preprocess_prog (), e2.preprocess_prog ());
   }
}

class Var extends GeometryExpression {
   private String s;

   Var (String s) {
      this.s = s;
   }

   public GeometryValue eval_prog (Map <String, GeometryExpression> env) {
      GeometryExpression res = env.get (s);
      if (res == null) {
         throw new NoElementException("undefined variable");
      }
      return res.eval_prog(env);
   }

   public GeometryExpression preprocess_prog () {
      return this;
   }
}

class Shift extends GeometryExpression {
   private double dx, dy;
   private GeometryExpression e;

   Shift (double dx, double dy, GeometryExpression e) {
      this.dx = dx;
      this.dy = dy;
      this.e = e;
   }

   public GeometryValue eval_prog (Map <String, GeometryExpression> env) {
      return (e.eval_prog(env)).shift(dx, dy);
   }

   public GeometryExpression preprocess_prog () {
      return new Shift (dx, dy, e.preprocess_prog());
   }
}

public class hw7 {
   public static void main (String[] args) {
      System.out.print("This is the challenge part for hw 7.");
   }
}