def calculate_female_probability(measurements, likelihood_female, likelihood_male, prior_female=0.5):
    """
    Calculate posterior probability of an individual being female using Bayes' theorem:
    
    P(Female|X) = P(X|Female) * P(Female) / P(X)
    
    where:
    - P(Female|X) is the posterior probability of being female given measurements
    - P(X|Female) is the likelihood of observing these measurements in females
    - P(Female) is the prior probability of being female
    - P(X) is the total probability of measurements (normalizing constant)
    """
    
    # Calculate evidence (total probability)
    evidence = (likelihood_female * prior_female + 
               likelihood_male * (1 - prior_female))
    
    # Calculate posterior probability using Bayes' theorem
    posterior_female = (likelihood_female * prior_female) / evidence
    
    return posterior_female
