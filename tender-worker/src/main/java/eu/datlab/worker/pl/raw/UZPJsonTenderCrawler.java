package eu.datlab.worker.pl.raw;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;
import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * UZP Tender JSON crawler.
 */
public final class UZPJsonTenderCrawler extends BaseDatlabIncrementalCrawler {

    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = PublicationSources.PL_UZP_JSON;
    private static final String NOTICE_SEARCH_URL = SOURCE_DOMAIN + "/BZP_PublicWebService.asmx/ogloszenia%sKryteriaWyszukiwaniaJSON?";

    private static final HashMap<String, String> FORM_TYPES = new HashMap<>();
    static {
        FORM_TYPES.put("ZP400", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_zamawiajacy_wojewodztwo=&_calkowita_wart_zam_od=-1" +
                "&_calkowita_wart_zam_do=-1");
        FORM_TYPES.put("ZP403", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
        FORM_TYPES.put("ZP404", "_rodzaj_zamawiajacego=99&_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
                "&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
//        FORM_TYPES.put("ZP405", "_rodzaj_zamawiajacego=99&_numer_ogloszenia=-1&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
//                "&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
        FORM_TYPES.put("ZP406", "_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_zamawiajacy_nazwa" +
                "=&_zamawiajacy_miejscowosc=");
        FORM_TYPES.put("ZP408", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_calkowita_wartosc_zamowieniaOd=-1&_calkowita_wartosc_zamowieniaDo=-1");
        FORM_TYPES.put("ZP409", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_data_zmiany_umowyOd=&_data_zmiany_umowyDo=");
        FORM_TYPES.put("ZP411", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_zamawiajacy_wojewodztwo=&_calkowita_wart_zam_od=-1" +
                "&_calkowita_wart_zam_do=-1");
    }

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("MM'%2F'dd'%2F'yyyy");
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2017, 7, 28);

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        for (String formType : FORM_TYPES.keySet()) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("formType", formType);
            createAndPublishMessage(String.format(NOTICE_SEARCH_URL + FORM_TYPES.get(formType), formType,
                    date.format(DATE_FORMATTER), date.format(DATE_FORMATTER)), metadata);
        }
    }

    @Override
    protected void finalCleanup() {
    }

    @Override
    protected LocalDate getDefaultStartDate() {
        return FIRST_DATE_AVAILABLE;
    }

    @Override
    protected ChronoUnit getIncrementUnit() {
        return ChronoUnit.DAYS;
    }

    @Override
    protected String getVersion() {
        return VERSION;
    }
}
