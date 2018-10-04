package eu.datlab.worker.pl.raw;

import eu.datlab.worker.raw.BaseDatlabIncrementalCrawler;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;

/**
 * UZP Tender JSON crawler.
 */
public final class UZPTenderJsonCrawler extends BaseDatlabIncrementalCrawler {

    private static final String VERSION = "1.0";

    private static final String SOURCE_DOMAIN = "http://websrv.bzp.uzp.gov.pl";
    private static final String NOTICE_SEARCH_URL = SOURCE_DOMAIN + "/BZP_PublicWebService.asmx/ogloszenia%sKryteriaWyszukiwaniaJSON?";
    private static final String AWARD_SEARCH_URL = SOURCE_DOMAIN + "/BZP_PublicWebService.asmx/ogloszenia%sKryteriaWyszukiwania_JSON?";
    private static final HashMap<String, String> NOTICE_FORM_TYPES = new HashMap<>();

    static {
        NOTICE_FORM_TYPES.put("ZP400", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_zamawiajacy_wojewodztwo=&_calkowita_wart_zam_od=-1" +
                "&_calkowita_wart_zam_do=-1");
        NOTICE_FORM_TYPES.put("ZP403", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
        NOTICE_FORM_TYPES.put("ZP404", "_rodzaj_zamawiajacego=99&_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
                "&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
        NOTICE_FORM_TYPES.put("ZP405", "_rodzaj_zamawiajacego=99&_numer_ogloszenia=-1&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
                "&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=");
        NOTICE_FORM_TYPES.put("ZP406", "_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_zamawiajacy_nazwa" +
                "=&_zamawiajacy_miejscowosc=");
        NOTICE_FORM_TYPES.put("ZP408", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_calkowita_wartosc_zamowieniaOd=-1&_calkowita_wartosc_zamowieniaDo=-1");
        NOTICE_FORM_TYPES.put("ZP409", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_data_zmiany_umowyOd=&_data_zmiany_umowyDo=");
        NOTICE_FORM_TYPES.put("ZP411", "_rodzaj_zamawiajacego=99&_rodzaj_zamowienia=99&_tryb_udzielenia_zamowienia=99&_numer_ogloszenia" +
                "=&_data_publikacjiOd=%s&_data_publikacjiDo=%s&_nazwa_nadana_zamowieniu=&_cpv_glowny_przedmiot=&_czyGrupaCPV=-1" +
                "&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc=&_zamawiajacy_wojewodztwo=&_calkowita_wart_zam_od=-1" +
                "&_calkowita_wart_zam_do=-1");
    }

    private static final HashMap<String, String> AWARD_FORM_TYPES = new HashMap<>();

    static {
        AWARD_FORM_TYPES.put("ZPK1", "_rodzaj_zamawiajacego=99&_rodzaj_umowy_koncesji=99&_numer_ogloszenia=&_data_publikacjiOd=%s" +
                "&_data_publikacjiDo=%s&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_koncesjonariusz_nazwa" +
                "=&_koncesjonariusz_miejscowosc=&_szacunkowa_wartosc_koncesji_od=-1&_szacunkowa_wartosc_koncesji_do=-1");
        AWARD_FORM_TYPES.put("ZPK2", "_rodzaj_zamawiajacego=99&_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
                "&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_zamawiajacy_nazwa=&_zamawiajacy_miejscowosc" +
                "=&_zamawiajacy_wojewodztwo=&_szacunkowa_wartosc_umowy_od=-1&_szacunkowa_wartosc_umowy_do=-1");
        AWARD_FORM_TYPES.put("ZPK3", "_rodzaj_umowy_koncesji=99&_numer_ogloszenia=&_data_publikacjiOd=%s&_data_publikacjiDo=%s" +
                "&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_koncesjonariusz_nazwa=&_koncesjonariusz_miejscowosc=");
        AWARD_FORM_TYPES.put("ZPK4", "_rodzaj_zamawiajacego=99&_rodzaj_umowy_koncesji=99&_numer_ogloszenia=&_data_publikacjiOd=%s" +
                "&_data_publikacjiDo=%s&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_koncesjonariusz_nazwa" +
                "=&_koncesjonariusz_miejscowosc=&_szacunkowa_wartosc_umowy_od=-1&_szacunkowa_wartosc_umowy_do=-1");
        AWARD_FORM_TYPES.put("ZPK5", "_rodzaj_zamawiajacego=99&_rodzaj_umowy_koncesji=99&_numer_ogloszenia=&_data_publikacjiOd=%s" +
                "&_data_publikacjiDo=%s&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_koncesjonariusz_nazwa" +
                "=&_koncesjonariusz_miejscowosc=&_szacunkowa_wartosc_umowy_od=-1&_szacunkowa_wartosc_umowy_do=-1");
        AWARD_FORM_TYPES.put("ZPK6", "_rodzaj_zamawiajacego=99&_rodzaj_umowy_koncesji=99&_numer_ogloszenia=&_data_publikacjiOd=%s" +
                "&_data_publikacjiDo=%s&_nazwa_przedmiotu_koncesji=&_glowny_kod_cpv=&_czyGrupaCPV=-1&_koncesjonariusz_nazwa" +
                "=&_koncesjonariusz_miejscowosc=&_data_zmiany_umowy_koncesji_od=&_data_zmiany_umowy_koncesji_do=");
    }

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("MM'%2F'dd'%2F'yyyy");
    private static final LocalDate FIRST_DATE_AVAILABLE = LocalDate.of(2016, 1, 1);

    @Override
    protected void initialSetup() {
    }

    @Override
    protected void crawlSourceForDate(final LocalDate date) {
        for (String formType : NOTICE_FORM_TYPES.keySet()) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("formType", formType);
            createAndPublishMessage(String.format(NOTICE_SEARCH_URL + NOTICE_FORM_TYPES.get(formType), formType,
                    date.format(DATE_FORMATTER), date.format(DATE_FORMATTER)), metadata);
        }

        for (String formType : AWARD_FORM_TYPES.keySet()) {
            @SuppressWarnings("unchecked") final HashMap<String, Object> metadata = new HashMap();
            metadata.put("formType", formType);
            createAndPublishMessage(String.format(AWARD_SEARCH_URL + AWARD_FORM_TYPES.get(formType), formType,
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
