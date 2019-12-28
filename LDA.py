#!/usr/bin/env python
# coding: utf-8

# In[28]:


'''
Cleaning and Preprocessing of the data
'''

# importing required libraries 
import nltk
nltk.download('stopwords')
nltk.download('wordnet')
from nltk.corpus import stopwords 
from nltk.stem.wordnet import WordNetLemmatizer
import string

# preparation of dataset
title1 = "Planning and Design Method of Land Consolidation in the Mountainous Region of Southeastern Hubei"
abstract1 = "This paper discussed the planning and design method of land consolidation in the mountainous region of southwestern Hubei with the land consolidation project of the low hilly regions in Wangying and Boyang Towns of Lichuan City in Hubei Province as an example.The land use problems of this area were analyzed,appropriate planning and design methods were found out for the main contradiction in land use,and under the guidance of the design objective and principles,a series of effective engineering measures and key project construction design were explored."
title2 = "Effect of a charged scanned probe microscope tip on a subsurface electron gas"
abstract2 = "Using a cryogenic scanned probe microscope (SPM) one can locally modify the sheet density of a two-dimensional electron gas (2DEG), and image the ballistic flow of electrons through a point contact in the 2DEG "
doc_complete = [title1, abstract1, title2, abstract2]
print('\n\nData\n\n')
print(doc_complete)

# set of stopwords
stop = set(stopwords.words('english'))
exclude = set(string.punctuation) 
lemma = WordNetLemmatizer()
def clean(doc):
    stop_free = " ".join([i for i in doc.lower().split() if i not in stop])
    punc_free = ''.join(ch for ch in stop_free if ch not in exclude)
    normalized = " ".join(lemma.lemmatize(word) for word in punc_free.split())
    return normalized
doc_clean = [clean(doc).split() for doc in doc_complete]    
print('\n\nCleaned Data\n\n')
print(doc_clean)


# In[29]:


# Importing Gensim
import gensim
from gensim import corpora

# Creating the term dictionary of our courpus, where every unique term is assigned an index. 
dictionary = corpora.Dictionary(doc_clean)

# Converting list of documents (corpus) into Document Term Matrix using dictionary prepared above.
doc_term_matrix = [dictionary.doc2bow(doc) for doc in doc_clean]


# In[33]:


# Creating the object for LDA model using gensim library
Lda = gensim.models.ldamodel.LdaModel

# Running and Trainign LDA model on the document term matrix.
ldamodel = Lda(doc_term_matrix, num_topics=10, id2word = dictionary, passes=50)


# In[34]:


print(ldamodel.print_topics(num_topics=10, num_words=10))


# In[ ]:




