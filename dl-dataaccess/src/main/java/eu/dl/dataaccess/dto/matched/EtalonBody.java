package eu.dl.dataaccess.dto.matched;

/**
 * Body used as etalon in exact and approximate matching plugin.
 *
 * @author Tomas Mrazek
 */
public interface EtalonBody extends ExactellyMatchable, ApproximatellyMatchable {
    /**
     * Identifier used for bodies created from etalon.
     */
    String ETALON_SOURCE_ID = "ETALON";

    /**
     * @return this instance transformed into MatchedBody
     */
    MatchedBody getAsMatchedBody();
}
