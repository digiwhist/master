package eu.datlab.dataaccess.dao.jdbc;

import eu.datlab.dataaccess.dao.MasterTenderZIndexIndicatorDAO;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.PublicationFormType;
import eu.dl.dataaccess.dto.master.MasterTender;

import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Master tender DAO for zindex indicators.
 */
public class JdbcMasterTenderZIndexIndicatorDAO extends JdbcMasterTenderDAO implements MasterTenderZIndexIndicatorDAO {
    @Override
    public final List<MasterTender> getBuyerTendersInPeriod(final String buyerGroupId, final LocalDate from, final LocalDate to,
                                                            final PublicationFormType formType) {

        try {
            PreparedStatement statement = connection.prepareStatement(
                "WITH rows AS ("
                    + " SELECT mt.*, min(p->>'publicationDate') AS first_published"
                    + " FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p"
                    + " WHERE data @> '{\"buyers\": [{\"groupId\": \"" + buyerGroupId + "\"}]}'"
                        + " AND (p->>'isIncluded')::boolean"
                        + " AND p @> '{\"formType\":\"" + formType + "\"}'"
                    + " GROUP BY mt.id)"
                    + " SELECT id, data, created, createdby, createdbyversion, modified, modifiedby, modifiedbyversion"
                    + " FROM rows"
                    + " WHERE first_published BETWEEN ? AND ?");

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
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final int[] getCPVMedianInPeriodBellowThreshold(final List<String> createdBy, final LocalDate from, final LocalDate to,
                                                           final String cpv, final BigDecimal threshold) {
        return getCPVMedianInPeriod(createdBy, from, to, cpv, threshold, false);
    }

    @Override
    public final int[] getCPVMedianInPeriodAboveThreshold(final List<String> createdBy, final LocalDate from, final LocalDate to,
                                                          final String cpv, final BigDecimal threshold) {
        return getCPVMedianInPeriod(createdBy, from, to, cpv, threshold, true);
    }

    /**
     * Returns median of the given CPV for tenders of the given type published in the given period and price bellow and/or above the
     * threshold.
     *
     * @param createdBy
     *      worker name
     * @param from
     *      start of the period
     * @param to
     *      end of the period
     * @param cpv
     *      CPV for that the median to be calculated
     * @param threshold
     *      lot.robustEstimatedPrice threshold
     * @param useAbove
     *      if TRUE take tenders with price above the threshold, otherwise bellow.
     * @return array where index 0 holds median value for the given CPV and index 1 holds number of tenders used for median calculation
     */
    private int[] getCPVMedianInPeriod(final List<String> createdBy, final LocalDate from, final LocalDate to, final String cpv,
                                             final BigDecimal threshold, final boolean useAbove) {

        if (createdBy == null || createdBy.isEmpty() || cpv == null) {
            return new int[]{0, 0};
        }

        String thresholdRestriction = "";
        if (threshold != null) {
            thresholdRestriction = " AND l#>>'{robustEstimatedPrice, netAmount}' IS NOT NULL" +
                " AND (l#>>'{robustEstimatedPrice, netAmount}')::decimal " + (useAbove ? ">" : "<=") + threshold;
        }

        try {
            String createdByRestriction = String.join(" OR ", createdBy.stream().map(n -> "mt.createdBy = '" + n + "'")
                .collect(Collectors.toList()));

            PreparedStatement statement = connection.prepareStatement(
                // select suited tenders
                "WITH"
                    + " tenders AS ("
                        + "	SELECT DISTINCT mt.*"
                        + "	FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p,"
                        + " jsonb_array_elements(mt.data#>'{cpvs}') c"
                        + "	WHERE (" + createdByRestriction + ")"
                            + " AND mt.data->>'procedureType' <> 'NEGOTIATED_WITHOUT_PUBLICATION'"
                            + " AND (p->>'isIncluded')::boolean"
                            + " AND p@>'{\"formType\":\"CONTRACT_AWARD\"}'"
                            + " AND p->>'publicationDate' BETWEEN ? AND ?"
                            + " AND (c->>'isMain')::boolean AND c->>'code' LIKE '" + cpv.replaceAll("\\-[0-9]$", "") + "%'"
                    + "),"
                    // select lot.bidsCount for each tender.lot and sort rows by count
                    + " bids AS ("
                        + " SELECT (l->>'bidsCount')::int AS bids_cnt, mt.id AS tender_id"
                        + " FROM tenders mt, jsonb_array_elements(mt.data#>'{lots}') l"
                        + "	WHERE l->>'bidsCount' IS NOT NULL"
                        + thresholdRestriction
                        + "	ORDER BY bids_cnt"
                    + "),"
                    // -- median calculation
                    + " counts AS ("
                        + "	SELECT array_agg(bids_cnt) AS bids_cnt, count(DISTINCT tender_id) AS tenders_cnt"
                        + "	FROM bids"
                    + ")"
                    + " SELECT CASE WHEN array_length(bids_cnt, 1) = 0 THEN 0"
                    + " WHEN array_length(bids_cnt, 1) % 2 = 0 THEN bids_cnt[array_length(bids_cnt, 1)/2]"
                    + " ELSE bids_cnt[(array_length(bids_cnt, 1)+1)/2] END AS median,"
                    + " tenders_cnt"
                    + " FROM counts");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());

            ResultSet rs = statement.executeQuery();
            int[] median;
            median = rs.next() ? new int[]{rs.getInt("median"), rs.getInt("tenders_cnt")} : new int[]{0, 0};

            rs.close();
            statement.close();

            return median;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    /**
     * Returns median of the given CPV for tenders of the given type published in the given period and price bellow and/or above the
     * threshold.
     *
     * @param createdBy
     *      worker name
     * @param from
     *      start of the period
     * @param to
     *      end of the period
     * @param cpv
     *      CPV for that the median to be calculated
     * @param threshold
     *      lot.robustEstimatedPrice threshold
     * @param useAbove
     *      if TRUE take tenders with price above the threshold, otherwise bellow.
     * @param eksWorker
     *      EKS master tender worker name
     * @return array where index 0 holds median value for the given CPV and index 1 holds number of tenders used for median calculation
     */
    private int[] getSKCPVMedianInPeriod(final List<String> createdBy, final LocalDate from, final LocalDate to, final String cpv,
                                             final BigDecimal threshold, final boolean useAbove, final String eksWorker) {

        if (createdBy == null || createdBy.isEmpty() || cpv == null) {
            return new int[]{0, 0};
        }

        String thresholdRestriction = "";
        if (threshold != null) {
            thresholdRestriction = " AND l#>>'{robustEstimatedPrice, netAmount}' IS NOT NULL" +
                " AND (l#>>'{robustEstimatedPrice, netAmount}')::decimal " + (useAbove ? ">" : "<=") + threshold;
        }

        try {
            String createdByRestriction = String.join(" OR ", createdBy.stream()
                .filter(n -> !n.equals(eksWorker))
                .map(n -> "mt.createdBy = '" + n + "'")
                .collect(Collectors.toList()));

            PreparedStatement statement = connection.prepareStatement(
                // select suited tenders
                "WITH"
                    + " tenders AS ("
                        + "	SELECT DISTINCT mt.*"
                        + "	FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p,"
                        + " jsonb_array_elements(mt.data#>'{cpvs}') c"
                        + "	WHERE (" + createdByRestriction + ")"
                            + " AND mt.data->>'procedureType' <> 'NEGOTIATED_WITHOUT_PUBLICATION'"
                            + " AND (p->>'isIncluded')::boolean"
                            + " AND p@>'{\"formType\":\"CONTRACT_AWARD\"}'"
                            + " AND p->>'publicationDate' BETWEEN ? AND ?"
                            + " AND (c->>'isMain')::boolean AND c->>'code' LIKE '" + cpv.replaceAll("\\-[0-9]$", "") + "%'"
                        + " UNION"
                        + "	SELECT DISTINCT mt.*"
                        + "	FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{publications}') p,"
                        + " jsonb_array_elements(mt.data#>'{cpvs}') c"
                        + "	WHERE mt.createdby = ?"
                            + " AND mt.data->>'procedureType' <> 'NEGOTIATED_WITHOUT_PUBLICATION'"
                            + " AND (p->>'isIncluded')::boolean"
                            + " AND p@>'{\"formType\":\"CONTRACT_AWARD\"}'"
                            + " AND (mt.data->>'awardDecisionDate')::date BETWEEN ? AND ?"
                            + " AND (c->>'isMain')::boolean AND c->>'code' LIKE '" + cpv.replaceAll("\\-[0-9]$", "") + "%'"
                    + "),"
                    // select lot.bidsCount for each tender.lot and sort rows by count
                    + " bids AS ("
                        + " SELECT (l->>'bidsCount')::int AS bids_cnt, mt.id AS tender_id"
                        + " FROM tenders mt, jsonb_array_elements(mt.data#>'{lots}') l"
                        + "	WHERE l->>'bidsCount' IS NOT NULL"
                        + thresholdRestriction
                        + "	ORDER BY bids_cnt"
                    + "),"
                    // -- median calculation
                    + " counts AS ("
                        + "	SELECT array_agg(bids_cnt) AS bids_cnt, count(DISTINCT tender_id) AS tenders_cnt"
                        + "	FROM bids"
                    + ")"
                    + " SELECT CASE WHEN array_length(bids_cnt, 1) = 0 THEN 0"
                    + " WHEN array_length(bids_cnt, 1) % 2 = 0 THEN bids_cnt[array_length(bids_cnt, 1)/2]"
                    + " ELSE bids_cnt[(array_length(bids_cnt, 1)+1)/2] END AS median,"
                    + " tenders_cnt"
                    + " FROM counts");

            statement.setString(1, from.toString());
            statement.setString(2, to.toString());
            statement.setString(3, eksWorker);
            statement.setString(4, from.toString());
            statement.setString(5, to.toString());

            ResultSet rs = statement.executeQuery();
            int[] median;
            median = rs.next() ? new int[]{rs.getInt("median"), rs.getInt("tenders_cnt")} : new int[]{0, 0};

            rs.close();
            statement.close();

            return median;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final int[] getSKCPVMedianInPeriodBellowThreshold(final List<String> createdBy, final LocalDate from, final LocalDate to,
                                                             final String cpv, final BigDecimal threshold, final String eksWorker) {
        return getSKCPVMedianInPeriod(createdBy, from, to, cpv, threshold, false, eksWorker);
    }

    @Override
    public final int[] getSKCPVMedianInPeriodAboveThreshold(final List<String> createdBy, final LocalDate from, final LocalDate to,
                                                            final String cpv, final BigDecimal threshold, final String eksWorker) {
        return getSKCPVMedianInPeriod(createdBy, from, to, cpv, threshold, true, eksWorker);
    }

    @Override
    public final List<MasterTender> getEKSTenders(final String organizationId, final LocalDate from, final LocalDate to,
                                                  final PublicationFormType formType, final String createdBy) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT DISTINCT mt.*"
                    + " FROM " + getTableWithSchema() + " mt, jsonb_array_elements(mt.data#>'{buyers}') b"
                    + " WHERE mt.createdby = ?"
                        + " AND b->>'groupId' IN ("
                            + "SELECT data->>'groupId'"
                            + " FROM " + schema + ".master_body"
                            + " WHERE data@>('{\"bodyIds\": [{"
                                + "\"id\": \"" + sanitizeForJsonString(organizationId) + "\","
                                     + "\"type\": \"" + BodyIdentifier.Type.ORGANIZATION_ID + "\"}]}')::jsonb)"
                        + " AND mt.data@>'{\"publications\": [{\"isIncluded\": true, \"formType\": \"" + formType + "\"}]}'"
                        + " AND (mt.data->>'awardDecisionDate')::date BETWEEN ? AND ?");

            statement.setString(1, createdBy);
            statement.setString(2, from.toString());
            statement.setString(3, to.toString());

            ResultSet rs = statement.executeQuery();

            List<MasterTender> result = new ArrayList<>();
            while (rs.next()) {
                result.add(createFromResultSet(rs));
            }

            rs.close();
            statement.close();

            return result;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final LocalDate getLastPublicationDate(final String createdBy, final String createdByVersion) {
        if (createdBy == null || createdByVersion == null) {
            return null;
        }

        try {
            PreparedStatement statement = connection.prepareStatement(
                "SELECT max(p->>'publicationDate') as last_date"
                + " FROM " + getTableWithSchema() + " t, jsonb_array_elements(t.data->'publications') p"
                + " WHERE createdby = ? AND createdbyversion = ?");

            statement.setString(1, createdBy);
            statement.setString(2, createdByVersion);

            ResultSet rs = statement.executeQuery();
            LocalDate lastDate = rs.next() ? rs.getDate("last_date").toLocalDate() : null;

            rs.close();
            statement.close();

            return lastDate;
        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}
