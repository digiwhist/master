
package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dto.matched.BVDEtalonBody;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.EtalonBodyDAO;
import eu.dl.dataaccess.dao.jdbc.BaseJdbcDAO;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.matched.MatchedGroupInfo;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * JDBC DAO implementation for BvD etalon bodies.
 * 
 * @author Kuba Krafka
 */
public class JdbcBVDEtalonBodyDAO extends BaseJdbcDAO<BVDEtalonBody> implements EtalonBodyDAO<BVDEtalonBody> {

    private static final int DIGEST_LENGTH_THRESHOLD = 3;

    /**
     * Initializes connection etc.
     */
    public JdbcBVDEtalonBodyDAO() {
        super();
    }

    @Override
    public final List<BVDEtalonBody> getExactMatchBodiesPool(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM bvd.registry_information WHERE " + restriction.toString() + ";");

            ResultSet rs = statement.executeQuery();

            List<BVDEtalonBody> result = new ArrayList<>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }

    @Override
    public final List<BVDEtalonBody> getApproximateMatchBodiesPool(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        if (digest == null || digest.length() <= DIGEST_LENGTH_THRESHOLD) {
            return Collections.emptyList();
        }

        StringBuilder restriction =
            approximateMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds, digest);

        if (restriction.length() == 0) {
            return Collections.emptyList();
        }

        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM bvd.registry_information WHERE " + restriction.toString() + ";");
            long pluginStartTime = System.currentTimeMillis();
            ResultSet rs = statement.executeQuery();
            long pluginEndTime = System.currentTimeMillis();
            logger.info("Approximate etalon selection took {} ms.",
                        pluginEndTime - pluginStartTime);

            List<BVDEtalonBody> result = new ArrayList<>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }

    @Override
    public final BVDEtalonBody getById(final String id) {
        try {
            PreparedStatement statement = getConnection().prepareStatement("SELECT * FROM bvd.registry_information  WHERE id = ?;");

            statement.setInt(1, Integer.valueOf(id));
            ResultSet rs = statement.executeQuery();

            BVDEtalonBody body = null;
            while (rs.next()) {
                body = createFromResultSet(rs);
            }

            rs.close();
            statement.close();

            return body;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }

    @Override
    public final List<BVDEtalonBody> findAll(final int pageNumber, final int pageSize) {
        return findAll(pageNumber, pageSize, 0);
    }

    @Override
    public final List<BVDEtalonBody> findAll(final int pageNumber, final int pageSize, final int offset) {
        try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM bvd.registry_information ORDER BY id LIMIT ? OFFSET ?;");
            
            statement.setInt(1, pageSize);
            statement.setInt(2, (pageNumber * pageSize) + offset);
            
            ResultSet rs = statement.executeQuery();

            List<BVDEtalonBody> result = new ArrayList<BVDEtalonBody>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }

    @Override
    public final void updateDigestsAndBodyIdsAndNuts(final BVDEtalonBody body) {
        try {
            PreparedStatement statement = getConnection().prepareStatement("UPDATE bvd.registry_information SET "
                + "digest = ?, standardizedname = ?, standardizedaddress = ?, nuts3 = ?, european_vat_number = ?,"
                + " statistical_number = ?, trade_register_number = ?, vattax_number = ?, digest2 = ? WHERE id = ?;");

            statement.setString(1, body.getDigest());
            statement.setString(2, body.getStandardizedName());
            statement.setString(3, body.getStandardizedAddress());
            statement.setString(4, body.getNuts3());
            statement.setString(5, body.getEuropeanVatNumber());
            statement.setString(6, body.getStatisticalNumber());
            statement.setString(7, body.getTradeRegisterNumber());
            statement.setString(8, body.getVatTaxNumber());
            statement.setString(9, body.getDigest2());
            statement.setInt(10, Integer.valueOf(body.getId()));
            statement.executeUpdate();

            statement.close();
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Creates BVDEtalonBody object from a result set object.
     * 
     * @param rs
     *            result set entry
     * @return initialized BVDEtalonBody
     * @throws SQLException
     *             in case retrieving values from rs failes
     * 
     */
    private BVDEtalonBody createFromResultSet(final ResultSet rs) throws SQLException {
        BVDEtalonBody body = new BVDEtalonBody();
        body.setId(rs.getString("id"));
        body.setStandardizedName(rs.getString("standardizedname"));
        body.setStandardizedAddress(rs.getString("standardizedaddress"));
        body.setName(rs.getString("name"));
        body.setPostcode(rs.getString("postcode"));
        body.setDigest(rs.getString("digest"));
        body.setCity(rs.getString("city"));
        body.setCountry(rs.getString("country"));
        body.setNuts3(rs.getString("nuts3"));
        body.setStreet(rs.getString("street_no_building_etc_line_1"));
        body.setEuropeanVatNumber(rs.getString("european_vat_number"));
        body.setVatTaxNumber(rs.getString("vattax_number"));
        body.setTradeRegisterNumber(rs.getString("trade_register_number"));
        body.setStatisticalNumber(rs.getString("statistical_number"));
        body.setBvdId(rs.getString("bvd_id_number"));
        body.setCountryIsoCode(rs.getString("country_iso_code"));

        body.setCompanyCategory(rs.getString("category_of_the_company"));
        body.setStatus(rs.getString("status"));
        body.setStatusChange(rs.getString("status_date"));
        body.setNace(rs.getString("nace_rev_2_core_code_4_digits"));
        body.setNumberOfEmployees(rs.getString("number_of_employees_last_value"));
        body.setFoundationDate(rs.getString("date_of_incorporation"));

        return body;
    }

    /**
     * Returns restriction for BvD etalon database query that returns etalons for exact matching. Null values of input
     * parameters aren't taken into account.
     *
     * @param standardizedName
     *      standardized name
     * @param standardizedAddress
     *      standardized address
     * @param bodyIds
     *      list of body identifiers
     * @return restriction as StringBuilder instance
     */
    private StringBuilder exactMatchRestrictionBuilder(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds) {

        StringBuilder restriction = new StringBuilder();
        if (standardizedName != null) {
            restriction.append("standardizedname = '").append(sanitize(standardizedName)).append("'");
        }
        if (standardizedAddress != null) {
            restriction
                .append(restriction.length() > 0 ? " OR " : "")
                .append("standardizedaddress = '").append(sanitize(standardizedAddress)).append("'");
        }
        if (bodyIds != null) {
            StringBuilder bodyIdsRestriction = new StringBuilder();

            bodyIds.stream().forEach((id) -> {
                if (id.getId() != null && id.getScope() != null) {
                    StringBuilder fieldRestriction = new StringBuilder("=")
                        .append("'").append(sanitize(id.getId())).append("'")
                        .append(" AND country_iso_code='").append(id.getScope()).append("'");

                    Arrays.asList("vattax_number", "trade_register_number", "statistical_number").stream()
                        .forEach(f -> { bodyIdsRestriction
                            .append(bodyIdsRestriction.length() > 0 ? " OR " : "")
                            .append("(").append(f).append(fieldRestriction).append(")");
                    });
                }
            });

            if (bodyIdsRestriction.length() > 0) {
                restriction
                    .append(restriction.length() > 0 ? " OR " : "").append(bodyIdsRestriction);
            }
        }

        return restriction;
    }

    /**
     * Returns restriction for BvD etalon database query that returns etalons for approximate matching. Null values of
     * input parameters aren't taken into account.
     *
     * @param standardizedName
     *      standardized name
     * @param standardizedAddress
     *      standardized address
     * @param bodyIds
     *      list of body identifiers
     * @param digest
     *      digest
     * @return restriction as StringBuilder instance
     */
    private StringBuilder approximateMatchRestrictionBuilder(final String standardizedName,
        final String standardizedAddress, final List<BodyIdentifier> bodyIds, final String digest) {

        StringBuilder restriction = exactMatchRestrictionBuilder(standardizedName, standardizedAddress, bodyIds);

        if (digest != null) {
            restriction
                .append(restriction.length() > 0 ? " OR " : "")
                .append("digest = '").append(sanitize(digest)).append("'");
        }

        return restriction;
    }

    @Override
    public final List<MatchedGroupInfo> getGroupsInfo(final List<String> groups) {
        // for etalons applies that each etalon is in own group
         return groups.stream().map(n -> new MatchedGroupInfo().setGroupId(n).setHasEtalon(true).setSize(1))
            .collect(Collectors.toList());
    }

	@Override
	public final List<BVDEtalonBody> findAllById(final int id, final int amount) {
		try {
            PreparedStatement statement = getConnection().prepareStatement(
                    "SELECT * FROM bvd.registry_information WHERE id >= ? ORDER BY id LIMIT ?;");
            
            statement.setInt(1, id);
            statement.setInt(2, amount);
            
            ResultSet rs = statement.executeQuery();

            List<BVDEtalonBody> result = new ArrayList<BVDEtalonBody>();

            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to generate result set, because of {}", e);
            throw new UnrecoverableException("Unable to generate result set.", e);
        }
    }
}
