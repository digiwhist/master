package eu.datlab.worker.py.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.py.DNCPFormType;
import eu.dl.dataaccess.dto.parsed.ParsedAmendment;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.parsed.ParsedTenderLot;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static eu.datlab.worker.py.parsed.DNCPParserUtils.parseIncludedPublication;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.path;
import static eu.datlab.worker.py.parsed.DNCPParserUtils.textValue;

/**
 * DNCP Amendment handler.
 *
 * @author Tomas Mrazek
 */
public final class DNCPAmendmentHandler {

    /**
     * Suppress default constructor.
     */
    private DNCPAmendmentHandler() {
    }

    /**
     * @param json
     *      JSON to be parsed
     * @param publicationDate
     *      publication date
     * @param url
     *      human readable url
     * @param metaData
     *      raw message metadata
     * @return list of parsed tenders
     */
    public static List<ParsedTender> parse(final JsonNode json, final String publicationDate, final String url,
                                           final Map<String, Object> metaData) {
        ParsedTender t = DNCPContractHandler.parse(json, publicationDate, url, metaData).get(0);
        // remove publications created by DNCPContractHandler
        t.setPublications(null);

        JsonNode data = path("releases/0/contracts/0", json);

        String contractId = textValue("extendsContractID", data);

        String amendmentType = textValue("dncpAmendmentType", data);

        // set DNCPAmendment publications
        t.addPublication(parseIncludedPublication(amendmentType == null ? DNCPFormType.AMENDMENT.name() : amendmentType,
                publicationDate, url, data))
            .addPublication(new ParsedPublication()
                .setSource(PublicationSources.PY_DNCP)
                .setIsIncluded(false)
                .setSourceId(contractId)
                .setSourceFormType(DNCPFormType.CONTRACT.name()))
            .addPublication(new ParsedPublication()
                .setSource(PublicationSources.PY_DNCP)
                .setIsIncluded(false)
                .setSourceId(textValue("awardID", data))
                .setSourceFormType(DNCPFormType.AWARD.name()))
            .setIsWholeTenderCancelled(String.valueOf("cancelled".equals(textValue("status", data))))
            .setModificationReason((String) metaData.get("descripcion"));

        ParsedAmendment amendment = null;

        if (amendmentType != null) {
            amendment = new ParsedAmendment();
            switch (amendmentType) {
                case "Renovación de Alquiler de Inmueble":
                case "Ampliación de Monto":
                    amendment.setUpdatedPrice(DNCPParserUtils.parsePrice(path("value", data)));
                case "Ampliación de Plazo":
                    String vigencia = (String) metaData.get("vigencia_contrato");
                    if (vigencia != null) {
                        if (vigencia.matches("(?i)^\\d+ Mes(es)?")) {               // 1 Mes, 12 Meses
                            amendment.setEstimatedDurationInMonths(vigencia);
                        } else if (vigencia.matches("(?i)^\\d+ Días?")) {         // 1 Día, 12 Días
                            amendment.setEstimatedDurationInDays(vigencia);
                        } else if (vigencia.matches("(?i)^\\d+ Años?")) {         // 1 Año, 12 Años
                            amendment.setEstimatedDurationInYears(vigencia);
                        }
                        // 11/06/2014 Meses
                        // 31/12/2014 Años
                        // 2013-11-01-2014-12-31
                        // 31-12-13 Días
                        // 31/12/2013 Días
                        // 2011 Años
                    }
                    break;
                case "Otras Modificaciones":
                    amendment.setDescription((String) metaData.get("descripcion"));
                    break;
                case "Reajuste de Precio":
                    amendment.setUpdatedPrice(DNCPParserUtils.parsePrice(path("value", data)));
                    break;
                default:
                    break;
            }
        }

        if (t.getLots() == null) {
            t.addLot(new ParsedTenderLot());
        }

        t.getLots().get(0)
            .setContractNumber(contractId)
            .addAmendment(amendment);

        return Arrays.asList(t);
    }    
}
