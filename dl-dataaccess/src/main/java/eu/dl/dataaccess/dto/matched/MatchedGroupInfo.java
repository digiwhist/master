package eu.dl.dataaccess.dto.matched;

/**
 * Class that holds information about group.
 */
public final class MatchedGroupInfo {
    private String groupId;

    private Integer size = 0;

    private Boolean hasEtalon = false;

    /**
     * @return group id
     */
    public String getGroupId() {
        return groupId;
    }

    /**
     * @param groupId
     *      group id to be set
     * @return this instance for chaining
     */
    public MatchedGroupInfo setGroupId(final String groupId) {
        this.groupId = groupId;
        return this;
    }

    /**
     * @return size of the group
     */
    public Integer getSize() {
        return size;
    }

    /**
     * @param size
     *      size of the group to be set
     * @return this instance for chaining
     */
    public MatchedGroupInfo setSize(final Integer size) {
        this.size = size;
        return this;
    }

    /**
     * @return true only and only if the group includes etalon body
     */
    public Boolean getHasEtalon() {
        return hasEtalon;
    }

    /**
     * @param hasEtalon
     *      whether group includes the etalon body
     * @return this instance for chaining
     */
    public MatchedGroupInfo setHasEtalon(final Boolean hasEtalon) {
        this.hasEtalon = hasEtalon;
        return this;
    }
}
