(ns liberator.builder
  (:require [liberator.core :as l]
            [liberator.conneg :as conneg])
  (:use clojure.tools.trace
        midje.sweet))

(defn is-request-method [methods context]
  #(some #{(get-in % [:request :request-method])} methods))

(defn wrap-fn
  ([x] "returns a function contantly evaluating x or x if it's a function"
     (if (fn? x) x (wrap-fn constantly x)))
  ([f x] ""
     (f x)))

(defmacro dedfn [name doc-string f & [args]]
  `(do (defn ~name  ~doc-string [~@args]
         [~(keyword name)
          ~(if args
             `#(~f ~@args %)
             (wrap-fn f))])
       (alter-meta! #'~name update-in [:arglists] conj [(symbol "function")])))

(dedfn service-available? "accepts function on context or a boolean value" wrap-fn [value])
(dedfn known-method?      "accepts function on context or list of known methods" is-request-method [methods])
(dedfn uri-too-long?      "accepts function on context or a boolean value" wrap-fn [value])
(dedfn method-allowed?    "accepts function on context or list of allowed methods " is-request-method [methods])
(dedfn malformed?         "accepts function on context or a boolean value" wrap-fn [value])

(defn negotiate-media-type [types]
  (fn [{:keys [request resource] :as context}]
    (when-let [type (liberator.conneg/best-allowed-content-type
                     (get-in request [:headers "accept"] "*/*")
                     types)]
      {:representation {:media-type (liberator.conneg/stringify type)}})))

(dedfn media-type-available? "accepts function context or list of available media types" negotiate-media-type [media-types])
(alter-meta! #'media-type-available? #(assoc % :arglists '([media-types] [function])))

(defn negotiate-language [languages]
   (fn [{:keys [request resource] :as context}]
    (when-let [language (liberator.conneg/best-allowed-language
                     (get-in request [:headers "accept-language"] "*")
                     languages)]
      {:representation {:language language}})))

(dedfn language-available? "accepts function context or list of available languages" negotiate-language [languages])
(alter-meta! #'language-available? #(assoc % :arglists '([languages] [function])))

(dedfn handle-ok
       "accepts a function generating a ring response of a constant value that is coerced into
        a ring response"
       wrap-fn [value])

(defn resource [& decisions]
  (let [m (into {} decisions)
        f (mapcat identity m)]
    (apply l/resource f)))

(defn as-media-type [s media-type]
  (condp = media-type
    "text/plain" s
    "text/html"  (format "<html>%s</html>" s)
    s))

;; canonical example. every decision is defined as a pair name-function
(fact
  (let [r (resource [:service-available?
                     (fn [ctx]
                       (not= (get-in ctx [:request :url]) "/disabled"))]
                    [:known-method?
                     (fn [{{m :request-method} :request}]
                       (some #{m} [:get :put :post :option :trace]))]
                    [:media-type-available?
                     (fn [{{{a "accept"} :headers} :request :as ctx}]
                       ;; todo make core/media-type-available work on f also,
                       ;; not only on list of media type
                       (if-let [t (conneg/best-allowed-content-type
                                   a ["text/html" "text/plain"])]
                         (assoc-in ctx [:representation :media-type] (conneg/stringify t))))]
                    [:language-available?
                     (fn [{{{al "accept-language"} :headers} :request}]
                       (conneg/best-allowed-language al ["de" "en"]))]
                    [:handle-ok
                     (fn [{{media-type :media-type :as representation} :representation}]
                       (as-media-type
                        (str  "The negotiated representation parameters are "
                              representation)
                        media-type))]
                    [:handle-not-acceptable
                     (fn [{:keys [represenation request]}]
                       (str "No representation available for the requested attributes: "
                            (select-keys (:headers request)
                                         ["accept" "accept-language"])))])]
    (r {:request-method :get})
    => {:body "The negotiated representation parameters are {}", :headers {"Content-Type" "text/plain"}, :status 200}
    (r {:request-method :get :headers {"accept" "text/plain"}})
    => {:body "The negotiated representation parameters are {:media-type \"text/plain\"}",
        :headers {"Content-Type" "text/plain" "Vary" "Accept"}, :status 200}
    (r {:request-method :get :headers {"accept" "text/rtf"}})
    => {:body "No representation available for the requested attributes: {\"accept\" \"text/rtf\"}",
        :headers {"Content-Type" "text/plain"}
        :status 406}))

;; make use of the syntactical helper
(def r2 (resource (service-available?
                   (fn [ctx] (not= (get-in ctx [:request :uri]) "/disabled")))
                  (known-method? [:get :put :post :option :trace])
                  (media-type-available? ["text/html" "text/plain"])
                  (language-available? ["de"])
                  (handle-ok #(str "The negotiated representation parameters are " (:representation %)))))


;; make use of conversion to function
(def r3 (resource (service-available?
                   (fn [ctx] (not= (get-in ctx [:request :uri]) "/disabled")))
                  (known-method? [:get :put :post :option :trace])
                  (media-type-available? ["text/html" "text/plain"])
                  (language-available? ["de"])
                  (handle-ok #(str "The negotiated representation parameters are " (:representation %)))))