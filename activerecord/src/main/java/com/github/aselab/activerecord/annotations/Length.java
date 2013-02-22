package com.github.aselab.activerecord.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.FIELD })
public @interface Length {
  int max() default Integer.MAX_VALUE;
  int min() default 0;
  String message() default "";
  String on() default "save";
}
