package it.polito.clj.languagespotter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Random;

import weka.classifiers.Classifier;
import weka.classifiers.bayes.HMM;
import weka.classifiers.bayes.NaiveBayes;
import weka.classifiers.evaluation.Evaluation;
import weka.core.Instances;
import weka.core.converters.ArffLoader.ArffReader;
import weka.core.converters.TextDirectoryLoader;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Reorder;
import weka.filters.unsupervised.attribute.StringToWordVector;

public class Learner {

	/**
	 * Object that stores training data.
	 */
	private Instances trainData;
	/**
	 * Object that stores the classifier
	 */
	private Classifier classifier = new NaiveBayes(); //HMM();

    private static final int NUM_ATTRIBUTES = 2;
			
	/**
	 * This method loads a dataset in ARFF format. 
	 */
	public void loadDataset(String name) {
		try {
			BufferedReader reader = new BufferedReader(new FileReader(name));
			ArffReader arff = new ArffReader(reader);
			trainData = arff.getData();
            if (trainData.numAttributes()!=NUM_ATTRIBUTES){
                throw new IllegalStateException("Train data expected to have "+NUM_ATTRIBUTES+" attribute, it has "+trainData.numAttributes());
            }
			System.out.println("===== Loaded dataset: " + name + " =====");
			reader.close();
		}
		catch (Exception e) {
			System.out.println("Problem found when reading: " + name);
            throw new RuntimeException(e);
		}
	}
	
	/**
	 * This method loads a data set from a directory. 
	 */
	public void loadDatasetFromDirectory(String name) {
		try {
			TextDirectoryLoader loader = new TextDirectoryLoader();
		    loader.setDirectory(new File(name));
			trainData = loader.getDataSet();
            if (trainData.numAttributes()!=NUM_ATTRIBUTES){
                throw new IllegalStateException("Train data expected to have "+NUM_ATTRIBUTES+" attribute, it has "+trainData.numAttributes());
            }
		
			System.out.println("===== Loaded dataset from folder: " + name + " =====");
		}
		catch (Exception e) {
			System.err.println("Problem found when reading from folder: " + name);
			throw new RuntimeException(e);
		}
	}
	
	/**
	 * This method evaluates the classifier. 
	 */
	public void evaluate() {
		try {
			if (null==trainData){
                throw new IllegalStateException("Train data not initialized");
            }
			Evaluation eval = new Evaluation(trainData);
			eval.crossValidateModel(classifier, trainData, 4, new Random(1));
			System.out.println(eval.toSummaryString());
			System.out.println(eval.toClassDetailsString());
			System.out.println("===== Evaluating on filtered (training) dataset done =====");
		}
		catch (Exception e) {
			System.err.println("Problem found while evaluating: "+e.getMessage());
			e.printStackTrace();
		}
	}
	
	/**
	 * This method trains the classifier on the loaded data set.
	 */
	public void learn() {
		try {
			StringToWordVector v_filter = new StringToWordVector();
			v_filter.setInputFormat(trainData);
            if (NUM_ATTRIBUTES!=trainData.numAttributes()){
                throw new IllegalStateException("Expected to have "+NUM_ATTRIBUTES+" attributes: "+trainData.numAttributes()+" found");
            }
			
			//word tokenizer
			/*v_filter.setOptions(
		            weka.core.Utils.splitOptions("-W 100000 " +
		                                         "-tokenizer \"weka.core.tokenizers.WordTokenizer\" "+ 
		                                         "-delimiters \"&[]//<>! \\r\\n\\t.,;:\\\'\\\"()?!{}\""));
			*/
			
			//lexical
			/*
			v_filter.setOptions(
					weka.core.Utils.splitOptions("-W 100000 " + 
				                                 "-tokenizer \"it.polito.clj.languagespotter.WekaLexicalProgrammingLanguageTokenizer\" " +
												 "-delimiters \"&[]//<>! \\r\\n\\t.,;:\\\'\\\"()?!{}\""));
			*/
			
			//structural
			v_filter.setOptions(
					weka.core.Utils.splitOptions("-W 100000 " + 
				                                 "-tokenizer \"it.polito.clj.languagespotter.WekaStructuralProgrammingLanguageTokenizer\" " +
												 "-delimiters \"&[]//<>! \\r\\n\\t.,;:\\\'\\\"()?!{}\""));
						
		    trainData = Filter.useFilter(trainData, v_filter);
		    	    
	        Reorder r = new Reorder();
	        r.setAttributeIndices("2-last,1");
	        r.setInputFormat(trainData);
	        trainData = Filter.useFilter(trainData, r);
		    //System.out.println(trainData);
	       
	        classifier.buildClassifier(trainData);
		    System.out.println("\n\nClassifier model:\n\n" + classifier);

			System.out.println("===== Training on filtered (training) dataset done =====");
		}
		catch (Exception e) {
			System.out.println("Problem found when training");
			throw new RuntimeException(e);
		}
	}
	
	/**
	 * This method saves the trained model into a file. This is done by
	 * simple serialization of the classifier object.
	 * @param fileName The name of the file that will store the trained model.
	 */
	public void saveModel(String fileName) {
		try {
			weka.core.SerializationHelper.write(fileName, classifier);
 			System.out.println("===== Saved model: " + fileName + " =====");
        } 
		catch (Exception e) {
			System.out.println("Problem found when writing: " + fileName);
		}
	}
	
	public static void main (String[] args) 
	{
		Learner learner = new Learner();
		learner.loadDatasetFromDirectory("/home/federico/temp/gist/YAML");
		learner.learn();
		learner.saveModel("profiles.model");
		learner.evaluate();
	}
}	