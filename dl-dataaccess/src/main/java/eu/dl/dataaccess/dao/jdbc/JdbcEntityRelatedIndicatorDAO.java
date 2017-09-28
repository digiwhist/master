package eu.dl.dataaccess.dao.jdbc;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dao.IndicatorDAO;
import eu.dl.dataaccess.dto.indicator.BasicEntityRelatedIndicator;

/**
 * JDBC implementation of basic entity indicator DAO.
 * 
 */
public class JdbcEntityRelatedIndicatorDAO extends GenericJdbcDAO<BasicEntityRelatedIndicator> 
    implements IndicatorDAO<BasicEntityRelatedIndicator> {

    private static final String TABLE_NAME = "indicator";


    @Override
    protected final String getTableWithSchema() {
        return schema + "." + TABLE_NAME;
    }

    @Override
    public final BasicEntityRelatedIndicator getEmptyInstance() {
        return new BasicEntityRelatedIndicator();
    }

    @Override
    public final void delete(final String entityId, final String indicatorType) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "DELETE FROM " + getTableWithSchema()
                            + " WHERE data @> '{ \"relatedEntityId\":\"" + sanitizeForJsonString(entityId) + "\"}' AND "
                            + "data @> '{ \"type\":\"" + sanitizeForJsonString(indicatorType) + "\"}'");

            statement.executeUpdate();

            statement.close();

        } catch (Exception e) {
            logger.error("Unable to perform query, because of of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }

    @Override
    public final List<BasicEntityRelatedIndicator> getByEntityId(final String id) {
        try {
            PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM " + getTableWithSchema() 
                    + " WHERE data @> '{ \"relatedEntityId\":\"" + sanitizeForJsonString(id) + "\"}'");

            ResultSet rs = statement.executeQuery();

            List<BasicEntityRelatedIndicator> result = new ArrayList<BasicEntityRelatedIndicator>();

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
    public final List<BasicEntityRelatedIndicator> getByEntityIds(final Collection<String> relatedEntityIds) {
        if (relatedEntityIds == null || relatedEntityIds.isEmpty()) {
            return null;
        }
        try {          
            List<BasicEntityRelatedIndicator> result = new ArrayList<BasicEntityRelatedIndicator>();
            Integer size = relatedEntityIds.size();
            Integer counter = 0;
            Integer pageSize = 100;
            ArrayList<String> list = new ArrayList<String>(relatedEntityIds);
            
            while (counter < size) {      
                String condition = new String();
                
                for (String id : list.subList(counter, Integer.min(size, counter + pageSize))) {
                    if (id != null) {
                        if (!condition.isEmpty()) {
                            condition = condition + " OR ";
                        } 
                        
                        condition = condition + "data @> '{ \"relatedEntityId\":\"" 
                        + sanitizeForJsonString(id) + "\"}'";
                    }
                }
                
                PreparedStatement statement = connection.prepareStatement(
                        "SELECT * FROM " + getTableWithSchema() 
                        + " WHERE " + condition + ";");

                ResultSet rs = statement.executeQuery();

                while (rs.next()) {
                    result.add(createFromResultSet(rs));
                }

                rs.close();
                statement.close();
                
                logger.debug("Selected {} indicators from {} to {}", pageSize, 
                        counter, Integer.min(size, counter + pageSize));
                
                counter = counter + pageSize;
            }
            return result;

        } catch (Exception e) {
            logger.error("Unable to perform query, because of {}", e);
            throw new UnrecoverableException("Unable to perform query.", e);
        }
    }
}