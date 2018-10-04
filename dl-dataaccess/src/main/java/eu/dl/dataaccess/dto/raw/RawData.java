package eu.dl.dataaccess.dto.raw;

import eu.dl.dataaccess.dto.StorableDTO;

import java.net.URL;
import java.util.Map;

/**
 * Raw data and metadata.
 */
public class RawData extends StorableDTO implements Raw {

    /**
     * URL which the data comes from.
     */
    private URL sourceUrl;

    /**
     * HTTP request POST parameters used to get the data.
     */
    private Map<String, Object> postParameters;

    /**
     * Downloaded raw data (eg. HTML source data, XML data, ...).
     */
    private String sourceData;

    /**
     * Downloaded raw data as binary data.
     */
    private byte[] sourceBinaryData;

    /**
     * MIME type of raw data.
     */
    private String sourceDataMimeType;

    /**
     * Source file name (useful eg. for files downloaded from FTP server).
     */
    private String sourceFileName;

    /**
     * Gets the source url.
     *
     * @return the source url
     */
    
    public final URL getSourceUrl() {
        return sourceUrl;
    }

    /**
     * Sets the source url.
     *
     * @param sourceUrl
     *            the new source url
     */
    public final void setSourceUrl(final URL sourceUrl) {
        this.sourceUrl = sourceUrl;
    }

    /**
     * Gets the post parameters.
     *
     * @return the post parameters
     */
    
    public final Map<String, Object> getPostParameters() {
        return postParameters;
    }

    /**
     * Sets the post parameters.
     *
     * @param postParameters
     *            the post parameters
     */
    public final void setPostParameters(final Map<String, Object> postParameters) {
        this.postParameters = postParameters;
    }

    /**
     * Gets the source data.
     *
     * @return the source data
     */
    
    public final String getSourceData() {
        return sourceData;
    }

    /**
     * Sets the source data.
     *
     * @param sourceData
     *            the new source data
     */
    public final void setSourceData(final String sourceData) {
        this.sourceData = sourceData;
    }

    /**
     * Gets the source binary data.
     *
     * @return the source binary data
     */
    
    public final byte[] getSourceBinaryData() {
        return sourceBinaryData;
    }

    /**
     * Sets the source binary data.
     *
     * @param sourceBinaryData
     *            the new source binary data
     */
    public final void setSourceBinaryData(final byte[] sourceBinaryData) {
        this.sourceBinaryData = sourceBinaryData;
    }

    /**
     * Gets the source data mime type.
     *
     * @return the source data mime type
     */
    
    public final String getSourceDataMimeType() {
        return sourceDataMimeType;
    }

    /**
     * Sets the source data mime type.
     *
     * @param sourceDataMimeType
     *            the new source data mime type
     */
    public final void setSourceDataMimeType(final String sourceDataMimeType) {
        this.sourceDataMimeType = sourceDataMimeType;
    }

    /**
     * Gets the source file name.
     *
     * @return the source file name
     */
    
    public final String getSourceFileName() {
        return sourceFileName;
    }

    /**
     * Sets the source file name.
     *
     * @param sourceFileName
     *            the new source file name
     */
    public final void setSourceFileName(final String sourceFileName) {
        this.sourceFileName = sourceFileName;
    }
}
