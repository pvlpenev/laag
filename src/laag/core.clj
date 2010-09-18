(ns laag.core)

(defn product [scalar vect]
  "scalar * vector"
  (map #(* scalar %) vect))

(defn difference [& rst]
  (reduce #(map - %1 %2) rst))

(defn crossproduct [a b]
  "vectorno proizvedenie"
  (map #(* %1 %2) a b))

(defn dotproduct [a b]
  "scalarno proizvedenie"
  (apply + (crossproduct a b)))

(defn proj [u v]
  "proekzia na v vurhu u"
  (product (/ (dotproduct v u)
	      (dotproduct u u))
	   u))

(defn ortogonalise [ort v]
  (apply difference (cons v (map #(proj % v) ort))))

(defn- gram-schmidt* [ort nonort]
  (cond (empty? nonort)
	ort
	(empty? ort)
	(recur [(first nonort)]
	       (rest nonort))
	:else (let [v (first nonort)
		    u (ortogonalise ort v)]
		(recur (conj ort (into [] u))
		       (rest nonort)))))

(defn gram-schmidt [& vecs]
  (gram-schmidt* [] vecs))

(defn magnitude [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn normalize [v]
  (product (/ 1 (magnitude v))
	   v))

(defn ortonormate [& vecs]
  (map normalize (apply gram-schmidt vecs)))

