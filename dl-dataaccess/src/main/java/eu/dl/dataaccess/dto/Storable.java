package eu.dl.dataaccess.dto;

import java.time.LocalDateTime;
import java.util.HashMap;

/**
 * Basic storable item.
 * 
 */
public interface Storable {
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
     * @return the created
     */
    LocalDateTime getCreated();

    /**
     * @param created
     *            the created to set
     */
    void setCreated(LocalDateTime created);

    /**
     * @return the modified
     */
    LocalDateTime getModified();

    /**
     * @param modified
     *            the modified to set
     */
    void setModified(LocalDateTime modified);

    /**
     * @return the createdBy
     */
    String getCreatedBy();

    /**
     * @param createdBy
     *            the createdBy to set
     */
    void setCreatedBy(String createdBy);

    /**
     * @return the modifiedBy
     */
    String getModifiedBy();

    /**
     * @param modifiedBy
     *            the modifiedBy to set
     */
    void setModifiedBy(String modifiedBy);

    /**
     * @return the createdByVersion
     */
    String getCreatedByVersion();

    /**
     * @param createdByVersion
     *            the createdByVersion to set
     */
    void setCreatedByVersion(String createdByVersion);

    /**
     * @return the modifiedByVersion
     */
    String getModifiedByVersion();

    /**
     * @param modifiedByVersion
     *            the modifiedByVersion to set
     */
    void setModifiedByVersion(String modifiedByVersion);

    /**
     * @return the metaData
     */
    HashMap<String, Object> getMetaData();

    /**
     * @param metaData
     *            the metaData to set
     */
    void setMetaData(HashMap<String, Object> metaData);
    
    /**
     * @return the persistentId
     */
    String getPersistentId();

    /**
     * @param persistentId the persistentId to set
     */
    void setPersistentId(String persistentId);

    /**
     * @return the processing order
     */
    String getProcessingOrder();

    /**
     * @param processingOrder the processing order to set
     */
    void setProcessingOrder(String processingOrder);
}
