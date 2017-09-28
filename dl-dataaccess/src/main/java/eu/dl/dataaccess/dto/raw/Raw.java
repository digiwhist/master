package eu.dl.dataaccess.dto.raw;

import java.net.URL;
import java.util.Map;

import eu.dl.dataaccess.dto.Storable;

/**
 * Shared interface for all raw items.
 */
public interface Raw extends Storable {
    /**
     * @return the id
     */
    String getId();

    /**
     * @param id
     *            the id to set
     */
    void setId(String id);

    /**
     * @return the sourceUrl
     */
    URL getSourceUrl();

    /**
     * @param sourceUrl
     *            the sourceUrl to set
     */
    void setSourceUrl(URL sourceUrl);

    /**
     * @return the postParameters
     */
    Map<String, Object> getPostParameters();

    /**
     * @param postParameters
     *            the postParameters to set
     */
    void setPostParameters(Map<String, Object> postParameters);

    /**
     * @return the sourceData
     */
    String getSourceData();

    /**
     * @param sourceData
     *            the sourceData to set
     */
    void setSourceData(String sourceData);

    /**
     * Gets source data as binary data.
     *
     * @return binary data
     */
    byte[] getSourceBinaryData();

    /**
     * Sets source data (eg. PDF file) as binary data.
     *
     * @param sourceBinaryData
     *            binary data to store
     */
    void setSourceBinaryData(byte[] sourceBinaryData);

    /**
     * Gets MIME type of source data.
     *
     * @return MIME type
     */
    String getSourceDataMimeType();

    /**
     * Sets MIME type of source data.
     *
     * @param sourceDataMimeType
     *            MIME type to store
     */
    void setSourceDataMimeType(String sourceDataMimeType);

    /**
     *
     * @return the name of source file
     */
    String getSourceFileName();

    /**
     *
     * @param sourceFileName
     *            the name of source file to set
     */
    void setSourceFileName(String sourceFileName);
    
    /**
     * @return the persistentId
     */
    String getPersistentId();

    /**
     * @param persistentId the persistentId to set
     */
    void setPersistentId(String persistentId);
}