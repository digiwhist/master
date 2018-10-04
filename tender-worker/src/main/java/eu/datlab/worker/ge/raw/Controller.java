package eu.datlab.worker.ge.raw;

import eu.datlab.dataaccess.dto.codetables.PublicationSources;

/**
 * Provides controller action urls.
 *
 * @author Tomas Mrazek
 */
public final class Controller {
    /**
     * Ajax controller actions.
     */
    public enum Action {
        /**
         * Tender summary fragment.
         */
        MAIN("app_main"),
        /**
         * Tender documents fragment.
         */
        DOCUMENTATION("app_docs"),
        /**
         * Bids and bidders fragment.
         */
        OFFERS("app_bids"),
        /**
         * Tender result fragment.
         */
        RESULT("agency_docs"),
        /**
         * Tender changelog fragment.
         */
        CHANGELOG("app_statushistory"),
        /**
         * Subject detail fragment.
         */
        PROFILE("profile"),
        /**
         * Subject list fragment.
         */
        RESULT_SET("search_app");

        /**
         * Value of url parameter action.
         */
        private final String value;

        /**
         * Constructor with value initialization.
         * @param value
         *      value of url parametr action for given action type
         */
        Action(final String value) {
            this.value = value;
        }

        /**
         * @return Value of parameter
         */
        public String getValue() {
            return value;
        }

        /**
         * Checks whether action returns tender detail fragment.
         *
         * @return decision whether action returns tender detail fragment
         */
        public boolean isTenderFragmetAction() {
            return !(equals(PROFILE) || equals(RESULT_SET));
        }
    }

    /**
     * Base url of controller.
     */
    public static final String CONTROLLER_URL = PublicationSources.GE_SPA + "/public/library/controller.php";

    /**
     * Suppress default constructor for noninstantiability.
     */
    private Controller() {
        throw new AssertionError();
    }

    /**
     * Returns action url that returns tender snippet for given id.
     * @param id
     *      id of tender
     * @param action
     *      action that returns tender detail fragment
     * @return tender snippet or null if given action is not supported
     */
    public static String getTenderSnippetUrl(final String id, final Action action) {
        if (!action.isTenderFragmetAction()) {
            return null;
        }
        return getControllerUrl(action, String.format("app_id=%s", id));
    }

    /**
     * Returns action url that returns subject detail snippet for given id.
     * @param id
     *      id of subject
     * @return subject detail snippet
     */
    public static String getSubjectSnippetUrl(final String id) {
        return getControllerUrl(Action.PROFILE, String.format("org_id=%s", id));
    }

    /**
     * Returns action url that returns list of tenders on given page.
     *
     * @param page
     *      page of results. Starting with 1.
     * @return list of results
     */
    public static String getResultSetUrl(final int page) {
        return getControllerUrl(Action.RESULT_SET, String.format("page=%s", page));
    }

    /**
     * Returns controller action url.
     *
     * @param action
     *      action of controller
     * @param additionalParams
     *      additonal parameters appropriate to action
     * @return controller action url
     */
    private static String getControllerUrl(final Action action, final String additionalParams) {
        StringBuilder query = new StringBuilder(String.format("?action=%s", action.getValue()));

        if (additionalParams != null && !additionalParams.isEmpty()) {
            query.append(String.format("&%s", additionalParams));
        }
        return CONTROLLER_URL + query.toString();
    }
}
