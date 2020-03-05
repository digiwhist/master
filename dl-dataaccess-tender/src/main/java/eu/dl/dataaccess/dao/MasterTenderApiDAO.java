package eu.dl.dataaccess.dao;

/**
 * Master tender DAO for API purposes. Combines matching and opentender support.
 */
public interface MasterTenderApiDAO extends MasterTenderMatchingDAO, MasterTenderOpentenderDAO {

}
