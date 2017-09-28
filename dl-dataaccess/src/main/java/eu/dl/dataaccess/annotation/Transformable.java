package eu.dl.dataaccess.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Used to annotate classes for which shoyld be API transformations applied. 
 *
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Transformable {

}