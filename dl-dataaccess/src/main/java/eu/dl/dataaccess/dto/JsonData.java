package eu.dl.dataaccess.dto;

import java.io.Serializable;

/**
 * POJO for JSON data.
 *
 */
public class JsonData implements Serializable {

    private static final long serialVersionUID = 2849865752424209859L;

    private String data;

    /**
     * @return the data
     */
    public final String getData() {
        return data;
    }

    /**
     * Sets the data.
     *
     * @param data
     *            the data to set
     * @return the json data
     */
    public final JsonData setData(final String data) {
        this.data = data;
        return this;
    }
}
