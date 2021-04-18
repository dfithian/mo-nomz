//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import UIKit
import Foundation

extension UIViewController {
    func loadUser(completion: ((CreateUserResponse) -> Void)?) {
        let spinner = startLoading()
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func loadIngredients(completion: ((ListIngredientResponse) -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(ListIngredientResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func mergeIngredients(ingredientIds: [Int], ingredient: ReadableIngredient, completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(MergeIngredientRequest(ids: ingredientIds, name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit, active: ingredient.active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func deleteIngredients(ingredientIds: [Int], completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteIngredientRequest(ids: ingredientIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func loadRecipes(completion: ((ListRecipeResponse) -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: {
                do {
                    let output = try JSONDecoder().decode(ListRecipeResponse.self, from: data!)
                    completion?(output)
                } catch {
                    self.defaultOnError(error)
                }
            })
        })
        task.resume()
    }
    
    func addRecipeLink(link: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/link")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            let onUnsuccessfulStatus = { (resp: URLResponse?) -> Void in self.alertUnsuccessful("Please paste or enter ingredients manually.")
            }
            self.withCompletion(data: data, resp: resp, error: error, completion: completion, onUnsuccessfulStatus: onUnsuccessfulStatus, onError: self.defaultOnError)
        })
        task.resume()
    }
    
    func addRecipeBody(name: String, content: String, completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/body")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeBodyRequest(name: name, content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func updateRecipe(id: Int, active: Bool, completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(UpdateRecipeRequest(id: id, active: active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
    
    func deleteRecipes(recipeIds: [Int], completion: (() -> Void)?) {
        let spinner = startLoading()
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue(state.apiToken, forHTTPHeaderField: "X-Mo-Nomz-API-Token")
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteRecipeRequest(ids: recipeIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            self.stopLoading(spinner)
            self.defaultWithCompletion(data: data, resp: resp, error: error, completion: completion)
        })
        task.resume()
    }
}
