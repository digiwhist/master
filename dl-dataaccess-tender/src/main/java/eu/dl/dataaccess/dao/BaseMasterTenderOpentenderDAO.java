package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterTender;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Master tender DAO with opentender support.
 */
public interface BaseMasterTenderOpentenderDAO {

    /**
     * Returns objects which has been modified after timestamp by certain source. The result is paged with {@code pageSize} records per
     * page.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param createdBy
     *            "author" of the change
     * @param countryCode
     *            country code
     * @param page
     *            order of the page in the result (for first page set 0)
     * @param opentender
     *          whether returns only opentender records (tender.metaData.opentender = true)
     * @param pageSize
     *      page size
     * @return set of objects modified after timestamp
     */
    List<MasterTender> getModifiedAfter(LocalDateTime timestamp, String createdBy, String countryCode, Integer page, boolean opentender,
                                        Integer pageSize);

    /**
     * Same as {@link BaseMasterTenderOpentenderDAO#getModifiedAfter(LocalDateTime, String, String, Integer, boolean, Integer)} but uses
     * default page size.
     *
     * @param timestamp
     *            objects modified after this timestamp will be returned
     * @param createdBy
     *            "author" of the change
     * @param countryCode
     *            country code
     * @param page
     *            order of the page in the result (for first page set 0)
     * @param opentender
     *          whether returns only opentender records (tender.metaData.opentender = true)
     * @return set of objects modified after timestamp
     */
    List<MasterTender> getModifiedAfter(LocalDateTime timestamp, String createdBy, String countryCode, Integer page, boolean opentender);

    /**
     * Returns count of objects which has been modified after timestamp.
     *
     * @param timestamp
     *            objects modified after this timestamp will be counted
     * @param createdBy
     *            "author" of the change
     * @param countryCode
     *            country code
     * @return count of objects modified after timestamp
     */
    Integer getModifiedAfterCount(LocalDateTime timestamp, String createdBy, String countryCode);
}
