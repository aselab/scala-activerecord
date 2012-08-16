package com.github.aselab.activerecord.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.FIELD })
public @interface Checked {
  String message() default "";
  String on() default "save";
}
