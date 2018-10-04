package eu.dl.dataaccess.dto.matched;

import eu.dl.dataaccess.dto.StorableDTO;

/**
 * Represents manual match between subjects.
 * 
 */
public class ManualMatch extends StorableDTO {
    private String groupId;

    private String fullHash;

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
     * Gets the full hash.
     *
     * @return the fullHash
     */
    public final String getFullHash() {
        return fullHash;
    }


    /**
     * Sets the hash.
     *
     * @param fullHash
     *            the new hash
     * @return the manual match
     */
    public final ManualMatch setFullHash(final String fullHash) {
        this.fullHash = fullHash;
        return this;
    }

}
