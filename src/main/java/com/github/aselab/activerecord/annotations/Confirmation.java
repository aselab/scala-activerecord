package com.github.aselab.activerecord.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.FIELD })
public @interface Confirmation {
  String value() default "";
  String message() default "";
  String on() default "save";
}
