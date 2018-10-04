package eu.dl.dataaccess.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import eu.dl.dataaccess.annotation.SystemProperty;
import eu.dl.dataaccess.annotation.Transformable;

import java.time.LocalDateTime;
import java.util.HashMap;


/**
 * Abstract class containing common metadata properties.
 */
@Transformable
public abstract class StorableDTO {
    /**
     * Object id - automatically generated unique identifier (surrogate key).
     */
    @JsonProperty("id")
    private String id;

    /**
     * Creation timestamp.
     */
    private LocalDateTime created;

    /**
     * Last modification timestamp.
     */
    private LocalDateTime modified;

    /**
     * Name of the worker that created the object.
     */
    private String createdBy;

    /**
     * Name of the last worker that modified the object.
     */
    private String modifiedBy;

    /**
     * Version of the worker that created the object.
     */
    private String createdByVersion;

    /**
     * Version of the worker that modified the object.
     */
    private String modifiedByVersion;

    /**
     * Metadata specific for this entry. Can be used to send additional info
     * between different phases.
     */
    private HashMap<String, Object> metaData;

    /**
     * Actual data are stored in this column.
     */
    private JsonData data;
    
    /**
     * Downloaded raw data (eg. HTML source data, XML data, ...).
     */
    private String persistentId;

    /**
     * Defines in which order should be this item processed.
     */
    private String processingOrder;

    /**
     * Gets the id.
     *
     * @return the id
     */
    public final String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id
     *            the new id
     */
    @JsonProperty("id")
    public final void setId(final String id) {
        this.id = id;
    }

    /**
     * Gets the created.
     *
     * @return the created
     */
    public final LocalDateTime getCreated() {
        return created;
    }

    /**
     * Sets the created.
     *
     * @param created
     *            the new created
     */
    public final void setCreated(final LocalDateTime created) {
        this.created = created;
    }

    /**
     * Gets the modified.
     *
     * @return the modified
     */
    public final LocalDateTime getModified() {
        return modified;
    }

    /**
     * Sets the modified.
     *
     * @param modified
     *            the new modified
     */
    public final void setModified(final LocalDateTime modified) {
        this.modified = modified;
    }

    /**
     * Gets the created by.
     *
     * @return the created by
     */
    public final String getCreatedBy() {
        return createdBy;
    }

    /**
     * Sets the created by.
     *
     * @param createdBy
     *            the new created by
     */
    public final void setCreatedBy(final String createdBy) {
        this.createdBy = createdBy;
    }

    /**
     * Gets the modified by.
     *
     * @return the modified by
     */
    public final String getModifiedBy() {
        return modifiedBy;
    }

    /**
     * Sets the modified by.
     *
     * @param modifiedBy
     *            the new modified by
     */
    public final void setModifiedBy(final String modifiedBy) {
        this.modifiedBy = modifiedBy;
    }

    /**
     * Gets the created by version.
     *
     * @return the created by version
     */
    public final String getCreatedByVersion() {
        return createdByVersion;
    }

    /**
     * Sets the created by version.
     *
     * @param createdByVersion
     *            the new created by version
     */
    @SystemProperty
    public final void setCreatedByVersion(final String createdByVersion) {
        this.createdByVersion = createdByVersion;
    }

    /**
     * Gets the modified by version.
     *
     * @return the modified by version
     */
    public final String getModifiedByVersion() {
        return modifiedByVersion;
    }

    /**
     * Sets the modified by version.
     *
     * @param modifiedByVersion
     *            the new modified by version
     */
    @SystemProperty
    public final void setModifiedByVersion(final String modifiedByVersion) {
        this.modifiedByVersion = modifiedByVersion;
    }

    /**
     * Gets the meta data.
     *
     * @return the meta data
     */
    public final HashMap<String, Object> getMetaData() {
        return metaData;
    }

    /**
     * Sets the meta data.
     *
     * @param metaData
     *            the meta data
     */
    public final void setMetaData(final HashMap<String, Object> metaData) {
        this.metaData = metaData;
    }

    /**
     * Data getter.
     * 
     * @return data
     */
    public final JsonData getData() {
        return data;
    }

    /**
     * Data setter.
     * 
     * @param data
     *            data
     */
    public final void setData(final JsonData data) {
        this.data = data;
    }
    
    /**
     * @return the persistentId
     */
    public final String getPersistentId() {
        return persistentId;
    }

    /**
     * @param persistentId the persistentId to set
     */
    @SystemProperty
    public final void setPersistentId(final String persistentId) {
        this.persistentId = persistentId;
    }

    /**
     * @return the processingOrder
     */
    public final String getProcessingOrder() {
        return processingOrder;
    }

    /**
     * @param processingOrder param
     */
    public final void setProcessingOrder(final String processingOrder) {
        this.processingOrder = processingOrder;
    }
}
