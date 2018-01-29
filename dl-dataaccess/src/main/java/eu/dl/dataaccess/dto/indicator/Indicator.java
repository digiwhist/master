package eu.dl.dataaccess.dto.indicator;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.util.HashMap;

/**
 * Indicator/red flag.
 * 
 */
@JsonTypeInfo(use= JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class",
        defaultImpl=BasicIndicator.class)
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
     * Returns value of the indicator.
     * 
     * @return indicator value
     */
    Double getValue();

    /**
     * Sets indicator value.
     *
     * @param value indicator value
     */
    void setValue(Double value);

    /**
     * Returns status of the indicator.
     * 
     * @return indicator status
     */
    IndicatorStatus getStatus();

    /**
     * Sets indicator status.
     *
     * @param status indicator status
     */
    void setStatus(IndicatorStatus status);
        	
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