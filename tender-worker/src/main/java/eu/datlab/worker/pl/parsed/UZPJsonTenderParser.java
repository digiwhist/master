package eu.datlab.worker.pl.parsed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.parser.BaseDatlabTenderParser;
import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;
import eu.dl.dataaccess.dto.parsed.ParsedBody;
import eu.dl.dataaccess.dto.parsed.ParsedCPV;
import eu.dl.dataaccess.dto.parsed.ParsedPrice;
import eu.dl.dataaccess.dto.parsed.ParsedPublication;
import eu.dl.dataaccess.dto.parsed.ParsedTender;
import eu.dl.dataaccess.dto.raw.RawData;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.parseItems;
import static eu.datlab.worker.pl.parsed.UZPJsonTenderUtils.textValue;

/**
 * Parser for UZP tenders in JSON.
 */
public class UZPJsonTenderParser extends BaseDatlabTenderParser {

    private static final String VERSION = "1.0";

    @Override
    protected final String countryOfOrigin(final ParsedTender parsed, final RawData raw) {
        return "PL";
    }

    @Override
    public final List<ParsedTender> parse(final RawData raw) {
        final HashMap<String, Object> metaData = raw.getMetaData();
        final String formType = (String) metaData.get("formType");

        JsonNode json;
        try {
            ObjectMapper mapper = new ObjectMapper();
            json = mapper.readTree(raw.getSourceData().replaceAll("<.*>", ""));
        } catch (IOException ex) {
            logger.error("Unable to parse JSON from raw data");
            throw new UnrecoverableException("Unable to parse JSON from raw data", ex);
        }

        final List<JsonNode> rawTenders = parseItems(json.findValue("Table"));
        final List<ParsedTender> parsedTenders = new ArrayList<>();

        if (rawTenders.isEmpty()) {
            logger.info("No tender data in JSON");
        }

        for (JsonNode rawTender : rawTenders) {
            final String isValid = textValue("ogloszenie_dotyczy", rawTender);
            final String guid = textValue(new String[]{"GuidId", "Guid" + formType}, rawTender);

            final ParsedTender parsedTender = new ParsedTender()
                    .setTitle(textValue("nazwa_nadana_zamowieniu", rawTender))
                    .setDescription(textValue("okreslenie_przedmiotu", rawTender))
                    .addPublication(new ParsedPublication()
                            .setSourceId(guid)
                            .setSourceTenderId(textValue("biuletyn", rawTender))
                            .setSource(PublicationSources.PL_UZP_JSON)
                            .setHumanReadableUrl(raw.getSourceUrl().toString())
                            .setPublicationDate(textValue("data_publikacji", rawTender))
                            .setIsValid(isValid != null && isValid.contains("1"))
                            .setSourceFormType(formType)
                            .setIsIncluded(true)
                    )
                    .addPublication(new ParsedPublication()
                            .setSourceId(textValue("zamowienie_bylo_przedmiotem_ogloszenia_pozycja", rawTender))
                            .setSourceFormType("notice")
                            .setIsIncluded(false)
                    )
                    .addBuyer(new ParsedBody()
                            .setName(textValue("zamawiajacy_nazwa", rawTender))
                            .setPhone(textValue("zamawiajacy_telefon", rawTender))
                            .setEmail(textValue("zamawiajacy_email", rawTender))
                            .setAddress(new ParsedAddress()
                                    .setStreet(textValue("zamawiajacy_adres_ulica", rawTender))
                                    .setCity(textValue("zamawiajacy_miejscowosc", rawTender))
                                    .setPostcode(textValue("zamawiajacy_kod_pocztowy", rawTender))
                                    .setCountry(textValue("zamawiajacy_panstwo", rawTender))
                                    .setState(textValue("zamawiajacy_wojewodztwo", rawTender))
                                    .setUrl(textValue("adres_strony_internetowej_url", rawTender))
                            )
                            .setBuyerType(textValue("rodzaj_zamawiajacego", rawTender))
                    )
                    .setIsCentralProcurement(textValue("czy_przeprowadza_centralny_zamawiajacy", rawTender))
                    .setIsJointProcurement(textValue("czy_przeprowadza_podmiot_zamawiajacy_powierzyl", rawTender))
                    .setSelectionMethod(textValue("tryb_udzielenia_zamowienia", rawTender))
                    .setNationalProcedureType(textValue("rodzaj_zamowienia", rawTender))
                    .addCpv(new ParsedCPV()
                            .setIsMain(Boolean.TRUE.toString())
                            .setCode(textValue(new String[]{
                                    "cpv_glowny_przedmiot",
                                    "glowny_kod_cpv"
                            }, rawTender))
                    )
                    .setIsElectronicAuction(textValue("aukcja_elektroniczna", rawTender))
                    .setHasLots(textValue("czy_podzielone_na_czesci", rawTender))
                    .setMaxBidsCount(textValue("maksymalna_liczba_czesci", rawTender))
                    .setEstimatedPrice(new ParsedPrice()
                            .setNetAmount(textValue("szacunkowa_wartosc_zamowienia_calosc", rawTender))
                            .setCurrency(textValue("waluta_calosc", rawTender))
                    )
                    .setEstimatedDurationInMonths(textValue("okres_w_miesiacach", rawTender))
                    .setEstimatedDurationInDays(textValue("okres_w_dniach", rawTender))
                    .setSelectionMethod(textValue("okreslenie_warunkow", rawTender))
                    .setTechnicalRequirements(textValue("zdolnosc_techniczna_informacje_dodatkowe", rawTender));

            if (formType.equals("ZP400")) {
                final HashMap<String, String> tenderMetaData = (HashMap<String, String>) metaData.get(guid);
                UZPJsonTenderNoticeHandler.parse(tenderMetaData.get("CRITERIA"), parsedTender);

            } else if (formType.equals("ZP403")) {
                final HashMap<String, String> tenderMetaData = (HashMap<String, String>) metaData.get(guid);
                UZPJsonTenderAwardHandler.parse(tenderMetaData.get("CONTRACTORS"), tenderMetaData.get("ATTACHMENTS"), parsedTender);
            }

            parsedTenders.add(parsedTender);
        }

        return parsedTenders;
    }

    @Override
    protected final String getVersion() {
        return VERSION;
    }

    @Override
    protected final List<ParsedTender> postProcessSourceSpecificRules(final List<ParsedTender> parsed, final RawData raw) {
        return parsed;
    }
}
