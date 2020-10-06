package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Master tender DAO with the method(s) for selection of tenders suitable for matching with other sources.
 */
public interface BaseMasterTenderMatchingDAO {
    /**
     * Returns suitable objects for matching with buyer profile tenders which has been modified after timestamp by certain source.
     * The result is paged with {@code pageSize} records per page. Excludes tenders published before 2012-06-30 or tenders without included
     * publication. If {@code createdBy} is null returns empty list.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param createdBy
     *            "author" of the change
     * @param page
     *            order of the page in the result (for first page set 0)
     * @param pageSize
     *            size of page
     * @return set of objects modified after timestamp or empty list
     */
    List<MasterTender> getModifiedAfterForBuyerProfileMatching(LocalDateTime timestamp, String createdBy, Integer page, Integer pageSize);
}
