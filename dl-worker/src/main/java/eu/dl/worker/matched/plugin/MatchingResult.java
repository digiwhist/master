package eu.dl.worker.matched.plugin;

import java.util.HashMap;

import eu.dl.dataaccess.dto.matched.MatchedBody;

/**
 * Matching result with the relevant info about matching.
 *
 */
public class MatchingResult {
    private Boolean matched = false;

    private String matchedBy;

    private String groupId;
    
    private String bodyId;
    
    private MatchedBody matchedBody;
    
    private HashMap<String, Object> metaData;

    /**
     * @return the bodyId
     */
    public final String getBodyId() {
        return bodyId;
    }

    /**
     * @param bodyId the bodyId to set
     */
    public final void setBodyId(final String bodyId) {
        this.bodyId = bodyId;
    }

    /**
     * @return the metaData
     */
    public final HashMap<String, Object> getMetaData() {
        return metaData;
    }

    /**
     * @param metaData the metaData to set
     */
    public final void setMetaData(final HashMap<String, Object> metaData) {
        this.metaData = metaData;
    }

    /**
     * @return the matched
     */
    public final Boolean getMatched() {
        return matched;
    }

    /**
     * @param matched
     *            the matched to set
     */
    public final void setMatched(final Boolean matched) {
        this.matched = matched;
    }

    /**
     * @return the matchedBy
     */
    public final String getMatchedBy() {
        return matchedBy;
    }

    /**
     * @param matchedBy
     *            the matchedBy to set
     */
    public final void setMatchedBy(final String matchedBy) {
        this.matchedBy = matchedBy;
    }

    /**
     * @return the groupId
     */
    public final String getGroupId() {
        return groupId;
    }

    /**
     * @param groupId
     *            the groupId to set
     */
    public final void setGroupId(final String groupId) {
        this.groupId = groupId;
    }

    /**
     * Getter.
     * @return matched body
     */
	public final MatchedBody getMatchedBody() {
		return matchedBody;
	}

	/**
	 * Setter.
	 * @param matchedBody matched body
	 */
	public final void setMatchedBody(final MatchedBody matchedBody) {
		this.matchedBody = matchedBody;
	}
}
