import multiprocessing as mp
import pandas as pd
import collections
import itertools
import operator
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import cmudict
prondict = cmudict.dict()
from langdetect import detect


class SimpleMapReduce(object):
    
    def __init__(self, map_func, reduce_func, num_workers=None):
        """
        map_func

          Function to map inputs to intermediate data. Takes as
          argument one input value and returns a tuple with the key
          and a value to be reduced.
        
        reduce_func

          Function to reduce partitioned version of intermediate data
          to final output. Takes as argument a key as produced by
          map_func and a sequence of the values associated with that
          key.
         
        num_workers

          The number of workers to create in the pool. Defaults to the
          number of CPUs available on the current host.
        """
        self.map_func = map_func
        self.reduce_func = reduce_func
        self.pool = mp.Pool(num_workers)
    
    def partition(self, mapped_values):
        """Organize the mapped values by their key.
        Returns an unsorted sequence of tuples with a key and a sequence of values.
        """
        partitioned_data = collections.defaultdict(list)
        for key, value in mapped_values:
            partitioned_data[key].append(value)
        return partitioned_data.items()
    
    def __call__(self, inputs, chunksize=1):
        """Process the inputs through the map and reduce functions given.
        
        inputs
          An iterable containing the input data to be processed.
        
        chunksize=1
          The portion of the input data to hand to each worker.  This
          can be used to tune performance during the mapping phase.
        """
        map_responses = self.pool.map(self.map_func, inputs, chunksize=chunksize)
        partitioned_data = self.partition(itertools.chain(*map_responses))
        reduced_values = self.pool.map(self.reduce_func, partitioned_data)
        return reduced_values

not_punctuation = lambda w: not (len(w)==1 and (not w.isalpha()))
get_word_count = lambda text: len(filter(not_punctuation, word_tokenize(text)))
get_sent_count = lambda text: len(sent_tokenize(text))
numsyllables_pronlist = lambda l: len(filter(lambda s: (s.encode('ascii', 'ignore').lower()[-1]).isdigit(), l))

def numsyllables(word):
    try:
        return list(set(map(numsyllables_pronlist, prondict[word.lower()])))
    except KeyError:
        return [0]

def text_statistics(text):
    word_count = get_word_count(text)
    sent_count = get_sent_count(text)
    syllable_count = sum(map(lambda w: max(numsyllables(w)), word_tokenize(text)))
    return word_count, sent_count, syllable_count

flesch_formula = lambda word_count, sent_count, syllable_count : 206.835 - (1.015*word_count/sent_count) - (84.6*syllable_count/word_count)


def flesch(word_count, sent_count, syllable_count):
    flesh = -1
    if (word_count>=1) and (sent_count>=1) and (syllable_count>=1):
        flesh= flesch_formula(word_count, sent_count, syllable_count)
    #print '{}-{}-{}-{} \n\n'.format(word_count,sent_count,syllable_count,flesh)
    return flesh
    
 
fk_formula = lambda word_count, sent_count, syllable_count : 0.39 *(word_count/sent_count) + 11.8*(syllable_count/word_count) - 15.59
def flesch_kincaid(word_count, sent_count, syllable_count):
    flesh = -1
    if (word_count>=1) and (sent_count>=1) and (syllable_count>=1):
        flesh = fk_formula(word_count, sent_count, syllable_count)
    return flesh


def file_to_words(filename):
    """Read a file and return a sequence (review_id, sentiment_text) values.
    """
    output = [tuple(x[0:2]) for x in filename.values if x[2]>0]
    return output


def count_words(item):
    """Convert the partitioned data for a word to a
    tuple containing the word and the number of occurances.
    """
    review_id, text = item
    cl_text = str(text[0]).decode('utf-8', 'ignore')
    word_count, sent_count, syllable_count = text_statistics(cl_text)
    language = 'NA'
    try:
	language = detect(cl_text)
    except Exception:
	pass
    return (review_id, text[0], language, flesch(word_count, sent_count, syllable_count), flesch_kincaid(word_count, sent_count, syllable_count))


if __name__ == '__main__':
    input_files = [pd.read_csv('SentiDF.csv')[['ReviewId','SentimentText','textcount']]]
    mapper = SimpleMapReduce(file_to_words, count_words, num_workers=3)
    word_counts = mapper(input_files, chunksize=5)
    pd.DataFrame(word_counts, columns=['ReviewId','SentimentText', 'language','flesch_reading_score','flesch_kincaid_score']).to_csv('readability_score.csv')