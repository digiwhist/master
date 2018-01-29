package eu.dl.dataaccess.dto.ocds;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.annotation.Transformable;

/**
 * Base class for OCDS extensions.
 *
 * @author Tomas Mrazek
 */
@Transformable
public abstract class BaseOCDSExtension {

    protected String url;

    /**
     * @return extension URL
     */
    @JsonIgnore
    abstract String getUrl();
}
