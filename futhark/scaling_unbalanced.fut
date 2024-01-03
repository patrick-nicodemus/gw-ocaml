module common (M : real) = {
  import "lib/github.com/diku-dk/sorts/radix_sort"
  type t = M.t
  def zero = M.i64 0
  def one = M.i64 1
  -- def pow a b = M.exp (b M.* (M.log a))
  -- pow is **.

  def scale (v : []t) (a :t) = map (M.* a) v
  def divv (v : []t) (a : t) = map (M./ a) v
  def add_vs (v : []t) (a :t) = map (M.+ a) v
  def sub_vs (v : []t) (a :t) = map (M.- a) v
  def odot u v= map2 (M.*) u v
  def dot v v' = reduce (M.+ ) zero (odot v v')
  def mat_mul [n][m][k] (A :[n][m]t) (B: [m][k]t) : [n][k]t =
    map (\a -> (map (dot a) (transpose B))) A
  def mat_vec K v = map (dot v) K
  def max_abs v = reduce M.max zero (map M.abs v)
  def err v v' = max_abs (map2 (M.-) v v')
  def ratio_err v v' = map2 (M./) v v' |> map (M.- one) |> max_abs
  def frobenius P Q = map2 dot P Q |> M.sum
  -- this is just called "maximum"
  -- def max1 = reduce M.max zero
  
  -- def sum = reduce (M.+) zero
  def sum_rows = map M.sum
  def avg [k] (a: [k](M.t)) = M.sum a M./ (M.i64 k)
  def sum_cols m = sum_rows (transpose m)
  def tensor [n] [m] (u : [n]t) (v : [m]t) : [n][m]t = map (scale v) u
  def replicate 'a (n : i64) (x : a) : [n]a = map (\_ -> x) (0..<n) 
  -- First dimension is number of points, second column is dimension.
  -- Each row is one point.
  def cdist_sq [n][m][k] (x : [n][k]M.t) (y : [m][k]M.t) : [n][m]M.t =
    let sqdist (a : [k]M.t) (b : [k]M.t) =
      let z = map2 (M.-) a b in
      dot z z
    in
    map (\xi -> map (sqdist xi) y) x
  -- First dimension is number of points, second column is dimension.
  -- Each row is one point.
  def pdist [n][k] (x : [n][k]t) : [n][n]t = cdist_sq x x |> map (map M.sqrt)

  def parallel_while 'a 'b [n] (update : a -> b -> a) (exit_condition : a -> bool)
    (varying_inputs : [n]a) (constant_inputs : [n]b) : [n]a =
    let (_, unsorted_results) = 
      loop (remaining, done) = (zip3 (0..<n) varying_inputs constant_inputs, [])
      while (length remaining) i64.> 0
      do
      let (rem_indices, rem_state, rem_constant) = unzip3 remaining in
      -- Note that update can contain a for loop if desired.
      let new = map2 update rem_state rem_constant
      let finished = map exit_condition new in
      let results = filter (\(a, _) -> a) (zip finished (zip rem_indices new))
		    |> map (\a -> a.1) in
      let next_inputs = filter (\(a,_)-> not a)
			       (zip finished (zip3 rem_indices new rem_constant)) |>
			map (\a -> a.1) in
      ( next_inputs , done ++ results)
    in
    let (_, return) =
      unzip (radix_sort_int_by_key (\a -> a.0) i64.num_bits i64.get_bit unsorted_results)
    in return :> [n]a

  -- klu is "un-normalized" or "unbiased", note the lack of -1 or + y.
  def klu x y = if x M.== zero then zero else x M.* M.log (x M./ y)
  -- KLu is the "un-normalized" KL divergence. 
  -- KLu(a | b) = \sum_i a_i ln(a_i/b_i)

  -- This is klu tweaked by adding 1e-10, which is deemed small.
  def klu_thibsej x y =
    x M.* M.log (x M./ y M.+ M.f64 1e-10) M.- x M.+ y

  def KLu [n] (a : [n]M.t) (b : [n]M.t) =
    M.sum (map2 klu a b)

  -- The "biased" version.
  def kl x y = if x M.== zero then y else x M.* M.log (x M./ y) M.- x M.+ y

  def KL [n] (a : [n]M.t) (b : [n]M.t) =
    M.sum (map2 kl a b)

  def KL2 [n][m] (p : [n][m]t) (q : [n][m]t) =
    map2 KL p q |> (M.sum)
  -- def KL2 [n][m] (p : [n][m]t) (q : [n][m]t) =
  --   map2 (map2 (\a b -> klu_thibsej a b)) p q |> map M.sum |> M.sum

  -- Identical to KLu2(P | \mu\otimes\nu) but with a different implementation
  -- that might be more performant.
  def KLu2 [n][m] (P : [n][m]t) (mu :[n]t) (nu : [m]t) : t =
    map2 (\mu_i P_i ->
	    (map2 (\nu_j P_ij ->
		     klu P_ij (mu_i M.* nu_j)) nu P_i |> M.sum)) mu P |> M.sum

  -- Thibault Sejourne modifies KL by 1e-10 to the log so it doesn't break.
  def KLu2_thibsej [n][m] (P : [n][m]t) (mu :[n]t) (nu : [m]t) : t =
    map2 (\mu_i P_i ->
	    (map2 (\nu_j P_ij ->
		     klu_thibsej
		     P_ij (mu_i M.* nu_j)) nu P_i |> M.sum)) mu P |> M.sum
      
  -- This is the expression from equation 6.
  def KL4 pi gamma mu nu =
    let massp = map M.sum pi |> M.sum in
    let massq = map M.sum gamma |> M.sum in
    let m_mnmn =
      let m_mu = M.sum mu in
      let m_nu = M.sum nu in
      let m_mn = m_mu M.* m_nu in
      m_mn M.* m_mn
    in
    (massq M.* KLu2 pi mu nu) M.+
    (massp M.* KLu2 gamma mu nu) M.-
    (massp M.* massq) M.+
    m_mnmn

  -- This is not identical to KL4, it adds a small fudge term to
  -- avoid having to deal with zeros.
  -- It should be, numerically, reasonably close to KL4.
  def KL4' pi gamma mu nu =
        let massp = map M.sum pi |> M.sum in
    let massq = map M.sum gamma |> M.sum in
    let m_mnmn =
      let m_mu = M.sum mu in
      let m_nu = M.sum nu in
      let m_mn = m_mu M.* m_nu in
      m_mn M.* m_mn
    in
    (massq M.* KLu2 pi mu nu) M.+
    (massp M.* KLu2 gamma mu nu) M.-
    (massp M.* massq) M.+
    m_mnmn


  def h = (\a -> if a M.== zero then zero else a M.* (M.log a M.- one))
  def H (p : [][]M.t) =
    M.sum (map (\row -> M.sum (map h row)) p)

}

-- This file deals with algorithms from the paper "Scaling algorithms
-- for unbalanced optimal transport"
-- by CHIZAT, GABRIEL PEYR´E, BERNHARD SCHMITZER,
-- AND FRANCOIS-XAVIER VIALARD

-- From page 22.
module scaling_unbalanced (M : real) = {
  def i32_plus = (i32.+)
  def i32geq = (i32.>=)
  def i64gt = (i64.>)

  open common M
  open M
  type otp [n][m] = { rho1 : t, rho2 : t, eps : t, mu : [n]t, nu : [m]t, C :[n][m]t }
  -- type otp0 [n][m] = { rho1 : t, rho2 : t, eps : t, mu : [n]t, nu : [m]t }

  -- Entropic cost. Defined as
  -- < C, P > + rho1 * KL(pi_1 P | mu) + rho2 * KL(pi_2 P | nu) + eps H(P)
  -- Double check your conventions about whether
  -- you need to use H(P) or KL(P | mu \otimes nu).
  -- This is subtle, minimizing H(P) biases us toward the uniform
  -- distribution, whereas
  -- minimizing KL(P | mu \otimes nu) biases us towards mu \otimes nu.
  def cost [n][m] (r : otp [n][m]) (P : [n][m]t) =
    (sum (map2 dot r.C P)) +
    ( r.eps * (H P)) +
    r.rho1 * (KL (map sum P) r.mu) +
    r.rho2 * (KL (map sum (transpose P)) r.nu)

  -- def costKL [n][m] (r : otp [n][m]) (P : [n][m]t) =
  --   sum (map2 dot r.C P) +
  --   r.eps * (KL2 P) +
  --   r.rho1 * (KL (map sum P) r.mu) +
  --   r.rho2 * (KL (map sum (transpose P)) r.nu)

  def dykstra_helper [n][m] (a : [n][m](M.t)) : ([n][m]M.t, [n]M.t)=
    let avgs = map avg a in
    let a' = map2 sub_vs a avgs in
    (a', avgs)

  type param = { exp_absorb_cutoff : t, tol_sinkhorn : t, tol_dykstra : t }

  -- Takes a matrix x and tries to write it as
  -- x = c + 1p^T + q1^T, where sum_rows c \approx sum_cols c \approx 0.
  -- This is useful because we want to solve optimal transport problems
  -- of the form e^{-C/\varepsilon}, where C may be large and \varepsilon
  -- may be small.
  def dykstra_matrix [n][m] (a : [n][m](M.t)) tol : ([n][m]M.t, [n]M.t, [m]M.t) =
    let (c_final, q_final, p_final0, p_final1) = 
      let (c1, q1) =
  	let (c1_t, q1) = dykstra_helper (transpose a) in
	(transpose c1_t, q1) in
      let (c2, p2) = dykstra_helper c1 in
      -- Loop invariant: c2 + p0 + p2' + q1 = x
      loop (c2 : [n][m]M.t, q1, p0, p2') = (c2, q1, map (\_ -> zero) (0..<n), p2)
      while max_abs p2' M.>= tol do
      let p2 = map2 (M.+) p0 p2' in
      let (c3, q3') =
	let (c3_t, q3') = dykstra_helper (transpose c2) in
	(transpose c3_t, q3') in
      let q3 = map2 (M.+) q1 q3' in
      let (c4, p4') = dykstra_helper c3 in
      (c4, q3, p2, p4') 
    in
    -- let _ = #[trace] map M.sum c_final |> M.maximum in
    -- let _ = #[trace] map M.sum (transpose c_final) |> M.maximum in
    (c_final, map2 (M.+) p_final0 p_final1, q_final)

  -- Returns (u, v, D) where
  -- C_ij = D_ij - u_i - v_j and
  -- u_i and v_ij are as large as possible such that
  -- no value of D_ij exceeds thres.
  -- 
  def safe_for_exp [n][m] (C : [n][m]t) thres : ([n][m]t,[n]t,[m]t) =
    let helper row = M.minimum (map (\a -> (thres M.- a) M./ M.i64 2) row) in
    let u = map helper C in
    let v = map helper (transpose C) in
    let D = map2 add_vs C u |> transpose |>
	    (\a -> map2 add_vs a v) |> transpose in
    
    -- let _ = #[trace] (map2 err C D) |> maximum in
    (D,u,v)
    
  -- PROBLEM:
  -- Have a linear cost matrix C \in R^{n\times m}
  -- Have two marginal penalty functions F1 and F2.
  -- F1 and F2 could be based, for example, on Kullback-Leibler divergence,
  -- or they could be infinite for P_X(R) not equal to the specified p,
  -- P_Y(R) not equal to some specified dy.
  -- Want to minimize over R for the entropy-regularized problem -
  -- < C,  R >_F + F1(P_X(R)) + F2(P_Y(R)) = eps * H(R).
  -- SOLUTION:
  -- First, absorb the cost matrix into the entropy term H.
  -- This implements the naive "Algorithm 1" from
  -- the paper.
  
  def algo1 [n] [m] proxdivF1 proxdivF2
    (dx : [n]t) (dy : [m]t) (K : ([n][m]t)) (eps : t) (tol : t) =
    let proxdivF1' b = proxdivF1 (mat_vec K (odot b dy)) eps in
    let proxdivF2' a = proxdivF2 (mat_vec (transpose K) (odot a dx)) eps in
    let b = replicate m one in
    let (a', b', _) =
      loop (a, b, tolab) = (proxdivF1' b, b, tol) while tolab M.>= tol do
        let a' = proxdivF1' b in
	let b' = proxdivF2' a' in
	let error = (err a a') M.+ (err b b') in
	(a', b', error)
    in
    let K1 = map2 scale K a' in
    let K2 = map2 scale (transpose K1) b' in 
    transpose K2

  def ext_sum [n] [m] (u : [n]t) (v :[m]t) : [n][m]t = map (add_vs v) u
  -- Computes the matrix K_ij = e^{ (u_i + v_j - C_ij) / eps }
   def stabilized_kernel [n] [m] (u: [n]t) (v : [m]t) (C : [n][m]t) eps :[n][m]t =
    (let uplusv = ext_sum u v in
    (let diff = map2 ( map2 (M.-) ) uplusv C in
    (map (map (\x -> M.exp (x M./ eps))) diff)))

  -- This implements "Algorithm 2" from the paper.
  def algo2 [n] [m] proxdivF1 proxdivF2 (dx : [n]t)
    (dy : [m]t) (C : ([n][m]t)) (eps : t) (tol : t)
    (absorption_cutoff : i64) : ([n]t, [m]t, [n]t, [m]t, [n][m]t, t) =
    let proxdivF1' (K_tilde : [n][m]t) (b_tilde : [m]t) (u : [n]t)  =
      proxdivF1 (mat_vec K_tilde (odot b_tilde dy)) u eps in
    let proxdivF2' (K_tilde : [n][m]t) a_tilde v =
      proxdivF2 (mat_vec (transpose K_tilde) (odot a_tilde dx)) v eps in
    let b_tilde = replicate m one in
    let u = replicate n zero in
    let v = replicate m zero in
    let K_tilde = stabilized_kernel u v C eps in    
    let a_tilde = proxdivF1' K_tilde b_tilde u in
    let toobig [k] (u: [k]t) (a:[k]t) : [k]t = map2 (M.+) u (map (M.* eps) a) in
    loop (a, b, u, v, K, error) : ([n]t, [m]t, [n]t, [m]t, [n][m]t, t)=
      (a_tilde, proxdivF2' K_tilde a_tilde v, u, v, K_tilde, tol)
    while error M.>= tol do
    let a' = proxdivF1' K_tilde b u in
    let b' = proxdivF2' K_tilde a' v in
    if
      (let log_compare x = M.log x M.>= M.i64 absorption_cutoff in
      any log_compare a' || any log_compare b')
    then
      let u' = toobig u a in
      let v' = toobig v b in
      let K' = stabilized_kernel u v C eps in
      let b' = replicate m one in
      (a', b', u', v', K', error)
    else
      (a', b', u, v, K,

       (err b' b) M.+ (err a a')
      )

  def count_nan m =
    map (\a -> if M.isnan a then (1:i32) else 0) m |> reduce (i32_plus) (0: i32) 
  def count_nan2d m = map count_nan m |> reduce i32_plus (0:i32)

  -- Returns abar (a = abar * e^ubar)
  def proxdiv3 [n] [m] (kbar : [n][m]M.t) (bbar : [m]M.t) (ubar : [n]M.t) (mu : [n]M.t) (eps : M.t) (rho1 : M.t) =
    let c1 = (M.neg rho1) M./ (rho1 M.+ eps) in
    let c2 = (M.neg eps) M./ (rho1 M.+ eps) in
    let kb = mat_vec kbar bbar in
    let kb_mu_pow = map2 (M./) kb mu |> map (\a -> a M.** c1)in
    let e_ubar_scale = map (M.* c2) ubar |> (map M.exp) in
    map2 (M.*) kb_mu_pow e_ubar_scale
    -- let _ = if isinf answer[36]
    -- 	    then #[trace] (c1, c2, kb[36], kb_mu_pow[36], e_ubar_scale[36])
    -- 	    else (c1, c2, kb[36], kb_mu_pow[36], e_ubar_scale[36]) in
    

  def rescale [m] upper_tol lower_tol ai ui ki : (M.t, M.t, [m]M.t)=
    -- let  _ = if isinf ui then #[trace] (ai, ki) else (ai, ki) in
    if (ai M.>= upper_tol) || (ai M.< lower_tol) then
    let lgai = M.log ai in
    -- let  _ = if isnan lgai then #[trace] (ai, ui, ki) else (ai, ui, ki) in
    let output = (sqrt ai, ui M.+ (lgai / M.i32 2), map (M.* (sqrt ai)) ki) in
    -- if i32geq (count_nan (map (M.* ai) ki)) (1 : i32) then
      -- let _ = #[trace] (ui, ai, lgai) in
      -- 	      #[trace] output
	    -- else
	      output 
    else
      (ai, ui, ki)
      
  def matrix_rescale upper_tol lower_tol a u k =
    let arr = map3 (rescale upper_tol lower_tol) a u k in
    unzip3 arr

  -- This is the main loop of algo3 and algo2, extracted for modularity.
  -- Our convention here is that Kbar[i][j]  == (exp ubar[i]) * K[i][j] * (exp vbar[j]).
  -- The input values of abar and bbar can be used to represent the best guess of the transport plan
  -- to use as a starting point.
  -- In the absence of a reasonable initial guess, we can just set them to one.
  -- def algo3_core_loop [n][m] (r : otp[n][m])
  type algo3_state [n][m] = { abar : [n]t, ubar :[n]t, bbar : [m]t, vbar :[m]t, Kbar : [n][m]t }

  def plan [n][m] (s : algo3_state[n][m]) =
    map2 scale s.Kbar s.abar |> transpose
    |> (\a -> map2 scale a s.bbar) |> transpose

  def normalize_state [n][m] (st : algo3_state[n][m]) (exp_absorb_cutoff :t) :
    algo3_state[n][m]=
    let (abar_n, ubar_n, kbar_n0) =
      -- let _ = if any (M.== zero) st.abar then #[trace] 1234567 else 0 in
      -- let _ = if any isinf st.abar then #[trace] st.abar else st.abar in
      if any (M.>= exp_absorb_cutoff) st.abar ||
	 any (M.<= M.recip exp_absorb_cutoff) st.abar then
	matrix_rescale exp_absorb_cutoff (M.recip exp_absorb_cutoff) st.abar st.ubar st.Kbar
      else
      -- let _ = #[trace] count_nan2d st.Kbar in
	(st.abar, st.ubar, st.Kbar)
    in
    -- let _ = if any (M.== zero) st.bbar then #[trace] 1234567 else 0 in
    let (bbar_n, vbar_n, kbar_n) = 
      if any (M.>= exp_absorb_cutoff) st.bbar ||
	 any (M.<= M.recip exp_absorb_cutoff) st.bbar then
      let (bbar2, vbar2, kbar2t) =
	matrix_rescale exp_absorb_cutoff (M.recip exp_absorb_cutoff) st.bbar st.vbar (transpose kbar_n0)
      in
      -- let _ = #[trace] count_nan2d kbar2t in

      (bbar2, vbar2, transpose kbar2t)
      else

	(st.bbar, st.vbar, kbar_n0)
    in
    { abar = abar_n, ubar = ubar_n, Kbar = kbar_n, bbar = bbar_n, vbar = vbar_n }


  -- def algo3_update_ip [n][m] (abar : (r : otp0) 

  def algo3_update [n][m] (st : algo3_state[n][m]) (r : otp[n][m])
    (exp_absorb_cutoff :t) : algo3_state [n][m]=
    -- let st1 = normalize_state st exp_absorb_cutoff in
    let bbar' = proxdiv3 (transpose st.Kbar) st.abar
			 st.vbar r.nu r.eps r.rho2 in
    let abar' = proxdiv3 st.Kbar bbar' st.ubar r.mu r.eps r.rho1 in
    normalize_state { abar = abar', vbar = st.vbar, bbar = bbar',
      ubar = st.ubar, Kbar = st.Kbar } exp_absorb_cutoff

  def algo3_core_loop [n][m] (st : algo3_state[n][m]) (r : otp[n][m])
    (params :param) =
    let (st1 : algo3_state[n][m]) = algo3_update st r params.exp_absorb_cutoff in
    loop (st0 : algo3_state[n][m], st1 : algo3_state[n][m]) = (st, st1) 
    while (ratio_err st0.abar st1.abar) M.>= params.tol_sinkhorn do
    let st2 = algo3_update st1 r params.exp_absorb_cutoff in
    (st1, st2)

  def algo3_core_loop_for [n][m] (st : algo3_state[n][m]) (r : otp[n][m])
    (max_cbar_val: t) (iter_count : i32) =
    loop (st: algo3_state[n][m]) = st 
    for i < iter_count do
    algo3_update st r max_cbar_val

  -- This algorithm is similar to algo2.
  -- It takes the same list of parameters.
  -- However, instead of calculating K directly, it first
  -- "normalizes" C by offloading some of the mass into row and
  -- column vectors. Algorithm 2 stops a and b from becoming extreme,
  -- but it does not account for the possibility that K may
  -- already be so small/large that one has immediate underflow/overflow.
  -- For readability I will hardcode this one to the
  -- unbalanced linear optimal transport problem.
  -- I can always refactor it later for greater generality.
  -- Another difference is that I have changed 
  -- from writing a = \tilde{a} * e^{\tilde{u}/\varepsilon} to a = \bar{a} * e^{\bar{u}}
  -- dropping varepsilon from the definition.
  def algo3 [n][m] (r : otp[n][m]) (params : param) =
    let c0 = map (map (M./ (M.neg r.eps))) r.C in
    -- let _ = #[trace] map count_nan c0 in
    -- let _ = #[trace] map (M.minimum) c0 in
  -- let (cbar, u0, v0) =
  --   -- #[trace]
  --   let (cbar, u0_neg, v0_neg) = dykstra_matrix c0 params.tol_dykstra in
  --   let _ = #[trace] 123 in
  --   let _ = #[trace] M.maximum (map M.maximum cbar) in
  --   let _ = #[trace] M.minimum (map M.minimum cbar) in
  --   (cbar, map (M.neg) u0_neg, map M.neg v0_neg)
  let (cbar, u0, v0) = safe_for_exp c0 (M.i64 30)
  in
  -- let _ = #[trace] map (M.minimum) cbar in
  -- let _ = #[trace] map (M.maximum) cbar in
  -- let _ = #[trace] M.minimum u0 in
  -- let _ = #[trace] M.maximum u0 in
  -- let _ = #[trace] M.minimum v0 in
  -- let _ = #[trace] M.maximum v0 in
  
  let kbar = map (map (M.exp)) cbar in
  let bbar = map (\_ -> one) (0..<m) in
  let (abar : [n]M.t) = proxdiv3 kbar bbar u0 r.mu r.eps r.rho1 in
  let (_,s) = algo3_core_loop
	  { abar = abar, ubar = u0, bbar = bbar, vbar = v0, Kbar = kbar }
	  r params
  in 
  (s.ubar, s.vbar, map2 scale s.Kbar s.abar |> transpose |> (\k -> map2 scale k s.bbar) |> transpose)

  -- Returns an initial state for the Sinkhorn loop,
  -- including a normalization of the input matrix that reduces it to a safe range.
  def init_state [n][m] (r : otp[n][m]) (abar0 : [n]t) (ubar0 : [n]t)
            (bbar0 : [m]t) (vbar0 : [m]t) : algo3_state[n][m] =
    let cbar =
      let c0 = map (map (M./ (M.neg r.eps))) r.C in
      let c1 = map2 (\a v -> map (a M.+) v) ubar0 c0 |> transpose in
      map2 (\a v -> map (a M.+) v) vbar0 c1 |> transpose
    in
    let (cbar', u', v') = safe_for_exp cbar (M.i64 30) in
    let kbar = map (map M.exp) cbar' in
    {abar = abar0, ubar = map2 (+) ubar0 u', bbar = bbar0,
     vbar = map2 (+) vbar0 v', Kbar = kbar }

  -- This improves upon the previous versions of the algorithm by considering
  -- that we may already have an estimate of the transport plan, i.e.,
  -- what are the appropriate (abar, ubar, bbar, vbar, Kbar).
  -- We do not need the original K if we have ubar, vbar and Kbar.
  -- One can always reconstruct K = Kbar/ubar * vbar^T.

  def algo4 [n][m] (r : otp[n][m]) (abar0 : [n]t) (ubar0 : [n]t)
            (bbar0 : [m]t) (vbar0 : [m]t) (params: param) =
    let s = init_state r abar0 ubar0 bbar0 vbar0 in
    -- let cbar =
    --   let c0 = map (map (M./ (M.neg r.eps))) r.C in
    --   let c1 = map2 (\a v -> map (a M.+) v) ubar0 c0 |> transpose in
    --   map2 (\a v -> map (a M.+) v) vbar0 c1 |> transpose
    -- in
    -- let kbar = map (map M.exp) cbar in
    let (_, st) = algo3_core_loop s (r : otp[n][m]) params
    in
    -- let _ = #[trace] count_nan st.abar in
    (map2 (M.+) st.ubar (map M.log st.abar),
     map2 (M.+) st.vbar (map M.log st.bbar),
     map2 scale st.Kbar st.abar |> transpose |>
     (\k -> map2 scale k st.bbar) |> transpose)

  def algo4_for [n][m] (r : otp[n][m]) (abar0 : [n]t) (ubar0 : [n]t)
            (bbar0 : [m]t) (vbar0 : [m]t) (max_cbar_val: t) iter_count =
    let s = init_state r abar0 ubar0 bbar0 vbar0 in
    -- let cbar =
    --   let c0 = map (map (M./ (M.neg r.eps))) r.C in
    --   let c1 = map2 (\a v -> map (a M.+) v) ubar0 c0 |> transpose in
    --   map2 (\a v -> map (a M.+) v) vbar0 c1 |> transpose
    -- in
    -- let kbar = map (map M.exp) cbar in
    let st = algo3_core_loop_for s (r : otp[n][m]) max_cbar_val iter_count
    in
    -- let _ = #[trace] count_nan st.abar in
    (map2 (M.+) st.ubar (map M.log st.abar),
     map2 (M.+) st.vbar (map M.log st.bbar),
     map2 scale st.Kbar st.abar |> transpose |>
     (\k -> map2 scale k st.bbar) |> transpose)
  -- -- Have c0_ij \approx cbar_ij + u0_i + v0_j.
  -- -- The rows of c0_ij sum to zero.
  -- -- The columns of c0_ij are *not* guaranteed to be within tol.
  -- let kbar = map (map (M.exp)) cbar in

  def prox_div_strict_s p s eps = p
  def prox_div_KLrho_s lambda p s eps =
    let c1 = eps M./ (lambda M.+ eps) in
    let c2 = lambda M./ (lambda M.+ eps) in
    odot
    (map (\x -> x M.** c1) s)
    (map (\x -> x M.** c2) p)

  def prox_div_strict_su p s u eps = map2 (M./) p s
  def prox_div_KLrho_su lambda p s u eps =
    let c1 = lambda M./(lambda M.+ eps) in
    let c2 = M.neg (lambda M.+ eps) in
    odot
    (map (\x -> x M.** c1) (map2 (M./) p s))
    (map M.exp (map (\x -> x M./ c2) u))

}

module pairs = {
  import "lib/github.com/diku-dk/segmented/segmented"
    -- This code is a slight modification of replicate iota in the stdlib.
  -- [n-1; n-2; n-3;...;0]
  def pairs (n : i64) =
    let reps = map (\k -> (n - 1) - k) (iota n) in
    let s1 = scan (+) 0 reps in
    let s2 = map (\i -> if i==0 then 0 else s1[i-1]) (iota n) in
    let tmp = scatter (replicate (reduce (+) 0 reps) 0) s2 (iota n) in
    let flags = map (>0) tmp in
    let fst = segmented_scan (+) 0 flags tmp in
    let snd' = segmented_iota flags in
    let snd = map2 (\a b-> a+b+1) fst snd' in
    zip fst snd

}

module unbalanced_gw (M : real) = {
  open common M
  module sinkhorn = scaling_unbalanced M
  type t = M.t
  -- ll = local linearization
  -- If T is a coupling matrix, A and B are distance matrices,
  -- a and b are the distributions, then
  -- this function computes L(A,B)\otimes T
  -- such that <L(A,B) \otimes T, T>_F is the classical GW cost of T
  -- (ignoring that T need not be a coupling, properly)
  -- Actually L2_otimes_T does not invoke the distributions.
  def L2_otimes_T [n][m] (A : [n][n]t) (B :[m][m]t) (P :[n][m]t) =
    let a = map M.sum P in
    let b = map M.sum (transpose P) in
    let X2x X x =
      map (\a -> dot (odot a a) x) X in
    let A2a = X2x A a in
    let B2b = X2x B b in
    let n2APB = mat_mul (mat_mul A P) B |> map (map (M.* (M.neg (M.i32 2)))) in
    let A2a_n2APB = map2 add_vs n2APB A2a in
    let A2a_B2b_n2APB = map2 add_vs (transpose A2a_n2APB) B2b |> transpose in
    A2a_B2b_n2APB

  -- Classical GW of the transport plan
  def GW_cost A B P =
    let a = map M.sum P in
    let b = map M.sum (transpose P) in
    let X2xx X x =
      map (\s -> dot (odot s s) x) X |> dot x in
    let A2aa = X2xx A a in
    let B2bb = X2xx B b in
    mat_mul (mat_mul A P) B |> frobenius P |>
    (\s -> (M.i64 (-2)) M.* s M.+ A2aa M.+ B2bb)

  -- Quadratic GW of two transport plans
  def GW_cost' A B P Q =
    let ap = map M.sum P in
    let bp = map M.sum (transpose P) in
    let aq = map M.sum Q in
    let bq = map M.sum (transpose Q) in
    let X2xy X x y =
      map (\s -> dot (odot s s) x) X |> dot y 
    in
    let A2aa' = X2xy A ap aq in
    let B2bb' = X2xy B bp bq in
        mat_mul (mat_mul A P) B |> frobenius Q |>
    (\s -> (M.i64 (-2)) M.* s M.+ A2aa' M.+ B2bb')
    
  -- Definition 1 in section 2.1
  def L rho1 rho2 X mu Y nu P =
    (GW_cost X Y P) M.+
    (let P1  = map M.sum P in
     rho1 M.* (KL2 (tensor P1 P1) (tensor mu mu))) M.+
    (let P2  = map M.sum (transpose P) in 
     rho2 M.* (KL2 (tensor P2 P2) (tensor nu nu)))

  def F [n][m] (A : [n][n]t) (B :[m][m]t) pi gamma mu nu rho1 rho2 =
    GW_cost' A B pi gamma M.+
    rho1 M.* KL2 (tensor (map M.sum pi)
			  (map M.sum gamma)) (tensor mu mu) M.+
    rho2 M.* KL2 (tensor (map M.sum (transpose pi))
			  (map M.sum (transpose gamma))) (tensor nu nu)
  -- An algebraic theorem (that we should test for correctness in the implementation!) is that
  -- D_phi rho1 (\pi1 \otimes \gamma1 ) (\mu\otimes\mu) ==
  -- rho1 * [ (KLu pi1 mu) * (m gamma) + (KLu gamma1 mu) * (m pi) - (m pi) * (m gamma) + (m mu)*(m mu)
  -- where m is the mass function.
  -- Of course we have the same for
  -- D_phi rho1 (\pi2 \otimes \gamma2 ) (\nu\otimes\nu).

  def Feps A B pi gamma mu nu rho1 rho2 eps =
    F A B pi gamma mu nu rho1 rho2
      M.+ eps M.* KL4 pi gamma mu nu
    
  def UGW_eps eps rho1 rho2 X mu Y nu P =
     (L rho1 rho2 X mu Y nu P) M.+
    eps M.* (KL4 P P mu nu)

  -- The UGW cost together with the constituent costs.
  def UGW_cost_arr eps rho1 rho2 X mu Y nu P =
    [GW_cost X Y P,
     let P1 = map M.sum P in KL2 (tensor P1 P1) (tensor mu mu),
     let P2 = map M.sum (transpose P) in KL2 (tensor P2 P2) (tensor nu nu),
     KL4 P P mu nu,
     UGW_eps eps rho1 rho2 X mu Y nu P]
    
  -- Feps <= UGW_eps
  -- Local linearization of the UGW gradient descent, cost matrix part
  -- X is a distance matrix, mu is a positive measure
  -- Y is a distance matrix, nu is a positive measure
  -- gamma is a measure on the product space.
  -- This is the expression c^\varepsilon_\gamma from Proposition 4.
  def ll_cost_matrix X mu Y nu gamma rho1 rho2 eps =
    let c1 = L2_otimes_T X Y gamma in
    let a1 = rho1 M.* KLu (map M.sum gamma) mu in
    let a2 = rho2 M.* KLu (map M.sum (transpose gamma)) nu in
    let a3 = eps M.* KLu2 gamma mu nu in
    map (\v -> add_vs v (a1 M.+ a2 M.+ a3)) c1

  -- Because of a convention change between the unbalanced Gromov-Wasserstein paper and
  -- the sinkhorn scaling paper, we need to modify the cost matrix slightly.
  -- This is the cost matrix which we actually want to use in the UGW descent loop.
  def ll_cost_matrix' X mu Y nu gamma rho1 rho2 eps =
    let epslgmu = map M.log mu |> map (M.* eps) in
    let epslgnu = map M.log nu |> map (M.* eps) in
    ll_cost_matrix X mu Y nu gamma rho1 rho2 eps |>
    (\a -> map2 sub_vs a epslgmu) |> transpose |> (\a -> map2 sub_vs a epslgnu) |> transpose


  -- This term describes the difference between the loss of the local linearization problem
  -- generated by ll_cost_matrix', and the value of F(P,Q) +\varepsilon KL(P\otimes Q | (\mu\otimes\nu)^2 ).
  -- Note that although p contains a field "C", the cost matrix,
  -- this does not play any role in the computation.
  def ll_error_term [n][m] Q (p :sinkhorn.otp[n][m]) =
    let m_mu = M.sum p.mu in
    let m_nu = M.sum p.nu in

    let t1 = (map M.sum Q |> M.sum |> M.neg) M.*
	     (p.rho1 M.* m_mu M.+ p.rho2 M.* m_nu) in
    let m_mu2 = m_mu M.* m_mu in
    let m_nu2 = m_nu M.* m_nu in
    let t2 = p.rho1 M.* m_mu2 M.+ p.rho2 M.* m_nu2 M.+ p.eps M.* m_mu2 M.* m_nu2 in
    t1 M.+ t2

  def count_nan v = map (\a -> if M.isnan a then 1 else 0) v |> i32.sum

  def unbalanced_gw_init_step [n][m] rho1 rho2 eps X mu Y nu gamma params =
    let c_eps_gamma = ll_cost_matrix' X mu Y nu gamma rho1 rho2 eps in
    -- let _ = #[trace] c_eps_gamma in
    let mass_gamma = map M.sum gamma |> M.sum in
    --Cost matrix for the unbalanced linear OT problem
    let C = map (map (M./ mass_gamma)) c_eps_gamma in
    -- let _ = #[trace] map count_nan C in
    let (r : sinkhorn.otp [n][m]) =
      { rho1 = rho1, rho2 = rho2,
	eps = eps, mu = mu, nu = nu, C = C }
    in
    let (u1, v1, C') = sinkhorn.algo3 r params in

    let sumC' = map M.sum C' |> M.sum in
    (u1, v1, map (map (M.* (M.sqrt (mass_gamma M./ sumC')))) C')

  def unbalanced_gw_descent_step [n][m] rho1 rho2 eps X mu Y nu u v
    (gamma : [n][m]t) params =
    let c_eps_gamma = ll_cost_matrix' X mu Y nu gamma rho1 rho2 eps in
    -- let _ = #[trace] c_eps_gamma in
    let mass_gamma = map M.sum gamma |> M.sum in
    --Cost matrix for the unbalanced linear OT problem
    let C = map (map (M./ mass_gamma)) c_eps_gamma in
    let (r : sinkhorn.otp [n][m]) =
      { rho1 = rho1, rho2 = rho2, eps = eps, mu = mu, nu = nu, C = C }
    in
    let (u1, v1, C') =
      sinkhorn.algo4 r (replicate n one) u (replicate m one) v params in
    let sumC' = map M.sum C' |> M.sum in
    (u1, v1, map (map (M.* (M.sqrt (mass_gamma M./ sumC')))) C')

  def unbalanced_gw_descent_step_for [n][m] rho1 rho2 eps X mu Y nu u v
    (gamma : [n][m]t) params iter_count =
    let c_eps_gamma = ll_cost_matrix' X mu Y nu gamma rho1 rho2 eps in
    -- let _ = #[trace] c_eps_gamma in
    let mass_gamma = map M.sum gamma |> M.sum in
    --Cost matrix for the unbalanced linear OT problem
    let C = map (map (M./ mass_gamma)) c_eps_gamma in
    let (r : sinkhorn.otp [n][m]) =
      { rho1 = rho1, rho2 = rho2, eps = eps, mu = mu, nu = nu, C = C }
    in
    let (u1, v1, C') =
      sinkhorn.algo4_for r (replicate n one) u (replicate m one) v params iter_count in
    let sumC' = map M.sum C' |> M.sum in
    let pi = map (map (M.* (M.sqrt (mass_gamma M./ sumC')))) C' in
    (u1, v1, pi, M.maximum (map2 ratio_err pi gamma))

  type problem_data [n][m] = {
      X : [n][n]t,
      mu : [n]t,
      Y : [m][m]t,
      nu : [m]t,
      rho1 : t,
      rho2 : t,
      eps : t
  }

  def ugw_descent_step_parallel [n][m] iter_count max_cbar_val
    (u, v, gamma, _)
    (const_data : problem_data[n][m]) =
    let { X, mu, Y, nu, rho1, rho2, eps } = const_data in
    let c_eps_gamma = ll_cost_matrix' X mu Y nu gamma rho1 rho2 eps in
    -- let _ = #[trace] c_eps_gamma in
    let mass_gamma = map M.sum gamma |> M.sum in
    --Cost matrix for the unbalanced linear OT problem
    let C = map (map (M./ mass_gamma)) c_eps_gamma in
    let (r : sinkhorn.otp [n][m]) =
      { rho1 = rho1, rho2 = rho2, eps = eps, mu = mu, nu = nu, C = C }
    in
    let (u1, v1, C') =
      sinkhorn.algo4_for r (replicate n one) u (replicate m one) v max_cbar_val iter_count in
    let sumC' = map M.sum C' |> M.sum in
    let pi = map (map (M.* (M.sqrt (mass_gamma M./ sumC')))) C' in
    (u1, v1, pi, M.maximum (map2 ratio_err pi gamma))
        
    def unbalanced_gw_parallel [k][n][m] rho1 rho2 eps
    (X: [k][n][n]t) (mu: [k][n]t) (Y: [k][m][m]t) (nu: [k][m]t)
    (gamma : [k][n][m]t) (max_cbar_val: t) iter_count tol_outerloop =
      let results = 
	let const_data : ([]problem_data[n][m]) =
	  map4 (\X mu Y nu -> { X, mu, Y, nu, rho1, rho2, eps }) X mu Y nu in
	let update = ugw_descent_step_parallel iter_count max_cbar_val in
	let variable_data =
	  map (\a -> (replicate n (M.i64 0), replicate m (M.i64 0), a, (M.i64 1))) gamma
	in
	let exit_condition a = a.3 M.< tol_outerloop in
	parallel_while update exit_condition variable_data const_data
      in
      let P = map (\a -> a.2) results in
      map5 (UGW_cost_arr eps rho1 rho2) X mu Y nu P

   def unbalanced_gw_init [n][m] rho1 rho2 eps X mu Y nu init params tol_outerloop =
    let (u0, v0, p0) =
       unbalanced_gw_init_step rho1 rho2 eps X mu Y nu init params in
    let update (u: [n]t) (v:[m]t) (p: [n][m]t) : ([n]t, [m]t, [n][m]t) =
      unbalanced_gw_descent_step rho1 rho2 eps X mu Y nu u v p params
    in
    -- let _ =
    --   #[trace] map count_nan p0 in
    loop (c0 : [n][m]t, u :[n]t, v:[m]t, c1:[n][m]t) = (init, u0, v0, p0)
    while any (M.>= tol_outerloop) (map2 err c0 c1)
    do
    -- let _ = #[trace] 444 in
    let (u', v', c2) = update u v c1 in
    -- let _ =  #[trace] Feps X Y c1 c2 mu nu rho1 rho2 eps in
    (c1, u', v', c2)

  def unbalanced_gw rho1 rho2 eps X mu Y nu =
    unbalanced_gw_init rho1 rho2 eps X mu Y nu (tensor mu nu)

  def unbalanced_gw_total_cost [n][m] rho1 rho2 eps (X : [n][n]t) mu (Y : [m][m]t) nu exp_absorb_cutoff tol_dykstra tol_sinkhorn tol_outerloop =
    let (_, _, _, P) =
      unbalanced_gw rho1 rho2 eps X mu Y nu
		    {exp_absorb_cutoff, tol_dykstra, tol_sinkhorn} tol_outerloop
    in
    UGW_eps eps rho1 rho2 X mu Y nu P

  def unbalanced_gw_pairwise [m][n][d] (pt_clouds: [m][n][d]t) rho1 rho2 eps
    exp_absorb_cutoff tol_dykstra tol_sinkhorn tol_outerloop =
       let dms = map pdist pt_clouds in
       let u = replicate n (M.recip (M.i64 n)) in
       let ugw (i,j) =
	 unbalanced_gw_total_cost
	 rho1 rho2 eps dms[i] u dms[j]
	 u exp_absorb_cutoff tol_dykstra tol_sinkhorn tol_outerloop
       in 
       map ugw (pairs.pairs m)

  def unbalanced_gw_pairwise_v2 [m][n][d]
    (pt_clouds: [m][n][d]t) rho1 rho2 eps
    max_cbar_val inner_count tol_outerloop =
    let a = (pairs.pairs m) in
    let dms = map pdist pt_clouds in
    let Xs = map (\(i,_) -> dms[i]) a in
    let Ys = map (\(_,j) -> dms[j]) a in
    let u = map (\_ -> (replicate n (M.recip (M.i64 n)))) a in
    let u_tensor_u =
      map (\_-> (replicate n (replicate n (M.recip (M.i64 (n*n)))))) a
    in
    unbalanced_gw_parallel rho1 rho2 eps Xs u Ys u u_tensor_u
			   max_cbar_val inner_count tol_outerloop

}

module unbalanced_gw64 = unbalanced_gw f64

entry unbalanced_gw_total_cost = unbalanced_gw64.unbalanced_gw_total_cost
entry unbalanced_gw_pairwise = unbalanced_gw64.unbalanced_gw_pairwise
entry unbalanced_gw_pairwise_v2 = unbalanced_gw64.unbalanced_gw_pairwise_v2
