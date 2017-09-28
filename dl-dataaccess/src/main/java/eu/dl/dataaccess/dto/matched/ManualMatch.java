package eu.dl.dataaccess.dto.matched;

import javax.persistence.Entity;
import javax.persistence.Table;

import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Represents manual match between subjects.
 * 
 */
@Entity
@Table(name = "manual_match")
public class ManualMatch extends StorableDTO {
    private String groupId;

    private String hash;

    private String flag;

    /**
     * Gets the flag.
     *
     * @return the flag
     */
    public final String getFlag() {
        return flag;
    }

    /**
     * Sets the flag.
     *
     * @param flag
     *            the flag to set
     * @return the manual match
     */
    public final ManualMatch setFlag(final String flag) {
        this.flag = flag;
        return this;
    }

    /**
     * Gets the group id.
     *
     * @return the group id
     */
    public final String getGroupId() {
        return groupId;
    }


    /**
     * Sets the group id.
     *
     * @param groupId
     *            the new group id
     * @return the manual match
     */
    public final ManualMatch setGroupId(final String groupId) {
        this.groupId = groupId;
        return this;
    }


    /**
     * Gets the hash.
     *
     * @return the hash
     */
    public final String getHash() {
        return hash;
    }


    /**
     * Sets the hash.
     *
     * @param hash
     *            the new hash
     * @return the manual match
     */
    public final ManualMatch setHash(final String hash) {
        this.hash = hash;
        return this;
    }

}
