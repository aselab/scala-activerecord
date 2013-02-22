package com.github.aselab.activerecord.sample;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target( { ElementType.FIELD })
public @interface CustomAnnotation {
  String value();
  String message() default "";
  String on() default "save";
}
