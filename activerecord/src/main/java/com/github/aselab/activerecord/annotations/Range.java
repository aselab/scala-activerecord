package com.github.aselab.activerecord.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.FIELD })
public @interface Range {
  double max() default Double.POSITIVE_INFINITY;
  double min() default Double.NEGATIVE_INFINITY;
  String message() default "";
  String on() default "save";
}
