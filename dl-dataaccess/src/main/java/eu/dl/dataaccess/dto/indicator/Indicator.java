package eu.dl.dataaccess.dto.indicator;

import java.util.HashMap;

/**
 * Indicator/red flag.
 * 
 */
public interface Indicator {

    /**
     * @return the type
     */
    String getType();

    /**
     * @param newType
     *            the type to set
     */
    void setType(String newType);

    /**
     * Gets the meta data.
     *
     * @return the meta data
     */
    HashMap<String, Object> getMetaData();

    /**
     * Sets the meta data.
     *
     * @param metaData
     *            the meta data
     */
    void setMetaData(HashMap<String, Object> metaData);
}