package eu.dl.dataaccess.dao;

import eu.dl.dataaccess.dto.master.MasterTender;

/**
 * Master tender DAO with the method(s) for selection of tenders suitable for matching with other sources.
 */
public interface MasterTenderMatchingDAO extends BaseMasterTenderMatchingDAO, MasterTenderDAO<MasterTender> {
}
