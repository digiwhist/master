package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;
import java.util.List;

import eu.dl.dataaccess.dto.matched.PoolBody;

/**
 * DAO for pool body.
 * 
 * @param <T>
 *            implementation class type that should be used for matched tender
 */
public interface PoolBodyDAO<T extends PoolBody> {
    
    /**
     * Returns information record for each group.
     *
     * @param groups
     *      groups
     * @return information about groups
     */
    List<MatchedGroupInfo> getGroupsInfo(List<String> groups);
}
