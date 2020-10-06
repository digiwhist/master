package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dao.MasterTenderIndicatorDAO;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * DAO for collecting data for computing indicator value.
 */
public class JdbcMasterTenderIndicatorDAO extends JdbcMasterTenderDAO implements MasterTenderIndicatorDAO {

    @Override
    public final List<MasterTender> getBuyerTendersInPeriod(final String buyerGroupId, final LocalDate from,
                                                      final LocalDate to) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(

                    "SELECT id, data, created, createdby, createdbyversion, modified, modifiedby, modifiedbyversion" +
                            " FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p" +
                            " WHERE data @> '{\"buyers\": [{\"groupId\": \"" + buyerGroupId + "\"}]}'" +
                            " AND (p->>'isIncluded')::boolean AND (p->>'publicationDate')::date BETWEEN ? AND ?" +
                            " GROUP BY mt.id");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());

            ResultSet rs = statement.executeQuery();

            List<MasterTender> result = new ArrayList<>();
            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<MasterTender> getSupplierTendersInPeriodByBuyer(final String buyerGroupId,
                                                                 final String supplierGroupId,
                                                                final LocalDate from, final LocalDate to) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT id, data, created, createdby, createdbyversion, modified, modifiedby, modifiedbyversion" +
                            " FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p," +
                            " jsonb_array_elements(mt.data#>'{lots}') l, jsonb_array_elements(l#>'{bids}') b," +
                            " jsonb_array_elements(b#>'{bidders}') bb" +
                            " WHERE data @> '{\"buyers\": [{\"groupId\": \"" + buyerGroupId + "\"}]}'" +
                            " AND b @> '{\"isWinning\":true}' AND bb @> '{\"groupId\": \"" + supplierGroupId + "\"}'" +
                            " AND (p->>'isIncluded')::boolean AND (p->>'publicationDate')::date BETWEEN ? AND ?" +
                            " GROUP BY mt.id");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());

            ResultSet rs = statement.executeQuery();

            List<MasterTender> result = new ArrayList<>();
            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
